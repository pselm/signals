
-- | The collage API is for freeform graphics. You can move, rotate, scale, etc.
-- | all sorts of forms including lines, shapes, images, and elements.
-- |
-- | Collages use the same coordinate system you might see in an algebra or physics
-- | problem. The origin (0,0) is at the center of the collage, not the top left
-- | corner as in some other graphics libraries. Furthermore, the y-axis points up,
-- | so moving a form 10 units in the y-axis will move it up on screen.

module Elm.Graphics.Collage
    ( --collage
      Form
    , toForm, filled, textured, gradient, outlined, traced, text, outlinedText
    , move, moveX, moveY, scale, rotate, alpha
    , group, groupTransform
    , Shape, rect, oval, square, circle, ngon, polygon
    , Path, segment, path
    , solid, dashed, dotted, LineStyle, LineCap(..), LineJoin(..), defaultLine
    ) where


import Elm.Color (Color, toCss, toCanvasGradient)
import Elm.Basics (Float)
import Elm.Text (Text)
import Elm.Transform2D (Transform2D)
import Elm.Transform2D (identity) as T2D
import Elm.Graphics.Element (Element)
import Elm.Graphics.Internal (createNode, setStyle)
import Elm.Color (Gradient, black)
import Math (pi, cos, sin)
import Data.List (List(..), (..), (:), snoc)
import Data.List.Zipper (Zipper(..), down)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import DOM (DOM)
import DOM.Node.Types (Element) as DOM
import DOM.Node.Element (setAttribute)
import DOM.HTML.Types (Window)
import DOM.HTML (window)
import Control.Monad.Eff (Eff, untilE)
import Control.Monad.ST (newSTRef, readSTRef, writeSTRef, runST)
import Control.Comonad (extract)
import Graphics.Canvas (Context2D, Canvas)
import Graphics.Canvas (LineCap(..), setLineWidth, setLineCap, setStrokeStyle, lineTo, moveTo, scale, stroke) as Canvas
import Control.Bind ((>=>))
import Math (sqrt)
import Prelude (class Eq, eq, pure, not, (<<<), (*), (/), ($), map, (+), (-), bind, (>>=), (<>), show, (<), (>), (&&), negate)


-- | A visual `Form` has a shape and texture. This can be anything from a red
-- | square to a circle textured with stripes.
newtype Form = Form
    { theta :: Number
    , scale :: Number
    , x :: Number
    , y :: Number
    , alpha :: Number
    , form :: BasicForm
    }


data FillStyle
    = Solid Color
    | Texture String
    | Grad Gradient

instance eqFillStyle :: Eq FillStyle where
    eq (Solid c1) (Solid c2) = eq c1 c2
    eq (Texture s1) (Texture s2) = eq s1 s2
    eq (Grad g1) (Grad g2) = eq g1 g2
    eq _ _ = false


-- | The shape of the ends of a line.
data LineCap
    = Flat
    | Round
    | Padded

instance eqLineCap :: Eq LineCap where
    eq Flat Flat = true
    eq Round Round = true
    eq Padded Padded = true
    eq _ _ = false


lineCap2Canvas :: LineCap -> Canvas.LineCap
lineCap2Canvas Flat = Canvas.Butt
lineCap2Canvas Round = Canvas.Round
lineCap2Canvas Padded = Canvas.Square


-- | The shape of the &ldquo;joints&rdquo; of a line, where each line segment
-- | meets. `Sharp` takes an argument to limit the length of the joint. This
-- | defaults to 10.
data LineJoin
    = Smooth
    | Sharp Float
    | Clipped

instance eqLineJoin :: Eq LineJoin where
    eq Smooth Smooth = true
    eq (Sharp f1) (Sharp f2) = eq f1 f2
    eq Clipped Clipped = true
    eq _ _ = false


-- TODO: Should suggest adding something like this to Graphics.Canvas
foreign import setLineJoinImpl :: ∀ e. String -> Number -> Context2D -> Eff (canvas :: Canvas | e) Context2D

-- | Set the current line join type.
setLineJoin :: ∀ e. LineJoin -> Context2D -> Eff (canvas :: Canvas | e) Context2D
setLineJoin Smooth = setLineJoinImpl "round" 10.0
setLineJoin (Sharp limit) = setLineJoinImpl "miter" limit
setLineJoin Clipped = setLineJoinImpl "bevel" 10.0


-- | All of the attributes of a line style. This lets you build up a line style
-- | however you want. You can also update existing line styles with record updates.
type LineStyle =
    { color :: Color
    , width :: Float
    , cap :: LineCap
    , join :: LineJoin
    , dashing :: List Int
    , dashOffset :: Int
    }


-- | The default line style, which is solid black with flat caps and sharp joints.
-- | You can use record updates to build the line style you
-- | want. For example, to make a thicker line, you could say:
-- |
-- |     defaultLine { width = 10 }
defaultLine :: LineStyle
defaultLine =
    { color: black
    , width: 1.0
    , cap: Flat
    , join: Sharp 10.0
    , dashing: Nil
    , dashOffset: 0
    }


-- | Create a solid line style with a given color.
solid :: Color -> LineStyle
solid clr =
    defaultLine
        { color = clr }


-- | Create a dashed line style with a given color. Dashing equals `[8,4]`.
dashed :: Color -> LineStyle
dashed clr =
    defaultLine
        { color = clr
        , dashing = (8 : 4 : Nil)
        }


-- | Create a dotted line style with a given color. Dashing equals `[3,3]`.
dotted :: Color -> LineStyle
dotted clr =
    defaultLine
        { color = clr
        , dashing = (3 : 3 : Nil)
        }


type Point =
    { x :: Float
    , y :: Float
    }


data BasicForm
    = FPath LineStyle (List Point)
    | FShape ShapeStyle (List Point)
    | FOutlinedText LineStyle Text
    | FText Text
    | FImage Int Int {top :: Int, left :: Int} String
    | FElement Element
    | FGroup Transform2D (List Form)


data ShapeStyle
    = Line LineStyle
    | Fill FillStyle


form :: BasicForm -> Form
form f =
    Form
        { theta: 0.0
        , scale: 1.0
        , x: 0.0
        , y: 0.0
        , alpha: 1.0
        , form: f
        }

fill :: FillStyle -> Shape -> Form
fill style (Shape shape) =
    form (FShape (Fill style) shape)


-- | Create a filled in shape.
filled :: Color -> Shape -> Form
filled color = fill (Solid color)


-- | Create a textured shape. The texture is described by some url and is
-- | tiled to fill the entire shape.
textured :: String -> Shape -> Form
textured src = fill (Texture src)


-- | Fill a shape with a gradient.
gradient :: Gradient -> Shape -> Form
gradient grad = fill (Grad grad)


{-| Outline a shape with a given line style. -}
outlined :: LineStyle -> Shape -> Form
outlined style (Shape shape) =
    form (FShape (Line style) shape)


-- | Trace a path with a given line style.
traced :: LineStyle -> Path -> Form
traced style (Path p) =
    form (FPath style p)


-- | Create a sprite from a sprite sheet. It cuts out a rectangle
-- | at a given position.
sprite :: Int -> Int -> {top :: Int, left :: Int} -> String -> Form
sprite a b c d = form $ FImage a b c d


-- | Turn any `Element` into a `Form`. This lets you use text, gifs, and video
-- | in your collage. This means you can move, rotate, and scale
-- | an `Element` however you want.
toForm :: Element -> Form
toForm e = form (FElement e)


-- | Flatten many forms into a single `Form`. This lets you move and rotate them
-- | as a single unit, making it possible to build small, modular components.
-- | Forms will be drawn in the order that they are listed, as in `collage`.
group :: List Form -> Form
group fs =
    form (FGroup T2D.identity fs)


-- | Flatten many forms into a single `Form` and then apply a matrix
-- | transformation. Forms will be drawn in the order that they are listed, as in
-- | `collage`.
groupTransform :: Transform2D -> List Form -> Form
groupTransform matrix fs =
    form (FGroup matrix fs)


-- | Move a form by the given amount (x, y). This is a relative translation so
-- | `(move (5,10) form)` would move `form` five pixels to the right and ten pixels up.
move :: Point -> Form -> Form
move {x, y} (Form f) =
    Form $ f
        { x = f.x + x
        , y = f.y + y
        }


-- | Move a shape in the x direction. This is relative so `(moveX 10 form)` moves
-- | `form` 10 pixels to the right.
moveX :: Float -> Form -> Form
moveX x (Form f) =
    Form $
        f { x = f.x + x }


-- | Move a shape in the y direction. This is relative so `(moveY 10 form)` moves
-- | `form` upwards by 10 pixels.
moveY :: Float -> Form -> Form
moveY y (Form f) =
    Form $
        f { y = f.y + y }


-- | Scale a form by a given factor. Scaling by 2 doubles both dimensions,
-- | and quadruples the area.
scale :: Float -> Form -> Form
scale s (Form f) =
    Form $
        f { scale = f.scale * s }


-- | Rotate a form by a given angle. Rotate takes standard Elm angles (radians)
-- | and turns things counterclockwise. So to turn `form` 30&deg; to the left
-- | you would say, `(rotate (degrees 30) form)`.
rotate :: Float -> Form -> Form
rotate t (Form f) =
    Form $
        f { theta = f.theta + t }


-- | Set the alpha of a `Form`. The default is 1, and 0 is totally transparent.
alpha :: Float -> Form -> Form
alpha a (Form f) =
    Form $
        f { alpha = a }


type Collage =
    { w :: Int
    , h :: Int
    , forms :: List Form
    }


-- | Create a collage with certain dimensions and content. It takes width and height
-- | arguments to specify dimensions, and then a list of 2D forms to decribe the content.
-- |
-- | Unlike with `Element`s, these 2D forms can be moved and rotated however you like.
-- | The forms are drawn in the order of the list, i.e., `collage w h (a : b : Nil)` will
-- | draw `b` on top of `a`.
-- TODO
--collage :: Int -> Int -> List Form -> Element
--collage =
--    Native.Graphics.Collage.collage


-- | A 2D path. Paths are a sequence of points. They do not have a color.
newtype Path = Path (List Point)


-- | Create a path that follows a sequence of points.
path :: List Point -> Path
path ps = Path ps


-- | Create a path along a given line segment.
segment :: Point -> Point -> Path
segment p1 p2 =
    Path (p1 : p2 : Nil)


-- | A 2D shape. Shapes are closed polygons. They do not have a color or
-- | texture, that information can be filled in later.
newtype Shape = Shape (List Point)


-- | Create an arbitrary polygon by specifying its corners in order.
-- | `polygon` will automatically close all shapes, so the given list
-- | of points does not need to start and end with the same position.
polygon :: List Point -> Shape
polygon = Shape


-- | A rectangle with a given width and height.
rect :: Float -> Float -> Shape
rect w h =
    let
        hw = w / 2.0
        hh = h / 2.0

    in
        Shape
            ( { x: 0.0 - hw, y: 0.0 - hh }
            : { x: 0.0 - hw, y: hh }
            : { x: hw, y: hh }
            : { x: hw, y: 0.0 - hh }
            : Nil
            )


-- | A square with a given edge length.
square :: Float -> Shape
square n = rect n n


-- | An oval with a given width and height.
oval :: Float -> Float -> Shape
oval w h =
    let
        n = 50
        t = 2.0 * pi / toNumber n
        hw = w / 2.0
        hh = h / 2.0

        f i =
            let
                ti = t * toNumber i

            in
                { x: hw * cos ti
                , y: hh * sin ti
                }

    in
        Shape $
            map f (0 .. (n - 1))


-- | A circle with a given radius.
circle :: Float -> Shape
circle r = oval (2.0 * r) (2.0 * r)


-- | A regular polygon with N sides. The first argument specifies the number
-- | of sides and the second is the radius. So to create a pentagon with radius
-- | 30 you would say:
-- |
-- |     ngon 5 30
ngon :: Int -> Float -> Shape
ngon n r =
    let
        t = 2.0 * pi / (toNumber n)
        f i =
            { x: r * cos (t * (toNumber i))
            , y: r * sin (t * (toNumber i))
            }

    in
        Shape $
            map f (0 .. (n - 1))


-- | Create some text. Details like size and color are part of the `Text` value
-- | itself, so you can mix colors and sizes and fonts easily.
text :: Text -> Form
text = form <<< FText


-- | Create some outlined text. Since we are just outlining the text, the color
-- | is taken from the `LineStyle` attribute instead of the `Text`.
outlinedText :: LineStyle -> Text -> Form
outlinedText ls t =
    form (FOutlinedText ls t)


-- RENDER


render :: ∀ e. Collage -> Eff (dom :: DOM | e) DOM.Element
render model = do
    div <- createNode "div"
    setStyle "overflow" "hidden" div
    setStyle "position" "relative" div
    -- update div model model
    pure div


setStrokeStyle :: ∀ e. LineStyle -> Context2D -> Eff (canvas :: Canvas | e) Context2D
setStrokeStyle style =
    Canvas.setLineWidth style.width >=>
    Canvas.setLineCap (lineCap2Canvas style.cap) >=>
    setLineJoin style.join >=>
    Canvas.setStrokeStyle (toCss style.color)

{-
	function setFillStyle(redo, ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}
 -}

-- Note that this traces first to last, whereas Elm traces from last to
-- first. If this turns out to matter, I can reverse the list.
trace :: ∀ e. Boolean -> List Point -> Context2D -> Eff (canvas :: Canvas | e) Context2D
trace closed list ctx =
    case list of
        Cons first rest -> do
            Canvas.moveTo ctx first.x first.y

            for_ rest \point ->
                Canvas.lineTo ctx point.x point.y

            if closed
                then Canvas.lineTo ctx first.x first.y
                else pure ctx

        _ ->
            pure ctx


line :: ∀ e. LineStyle -> Boolean -> List Point -> Context2D -> Eff (canvas :: Canvas | e) Context2D
line style closed pointList ctx =
    case pointList of
        -- Note that elm draws from the last point to the first, whereas we're
        -- drawing from the first to the last. If this turns out to matter, we
        -- can always start by reversing the list.
        Cons firstPoint remainingPoints -> do

            -- We have some points. So, check the dashing.
            case style.dashing of
                Cons firstDash remainingDashes ->

                    -- We have some dashing.
                    -- Note that Elm implements the Canvas `setLineDash` manually, perhaps for
                    -- back-compat with IE <11. So, we'll do it too!

                    -- The rest of this is easiest, I'm afraid, if we allow a bit of
                    -- mutation. At least, at a first approximation. I should probably
                    -- put all of this inside a state monad, but I'll try runST first.
                    -- I suppose that what's really going on here is that we're iterating
                    -- through both the points and the dashes, and each has to give up
                    -- control to the other at certain points. So, probably the right way
                    -- to do this would be with continuations. But that might be overkill.
                    runST do

                        -- The dashes are basically a list of on/off lengths which we
                        -- want to iterate through, and then go back to the beginning.
                        -- I think this is easiest with a zipper, though in fact you
                        -- could construct something even more specific. We remember
                        -- the firstPattern so we can go back to the beginning easily.
                        let firstPattern = Zipper Nil firstDash remainingDashes
                        pattern <- newSTRef firstPattern

                        -- We also need to keep track of how much is left in the current
                        -- pattern of the pattern ... that is, how much length we can
                        -- draw or skip until we should look at the next segment
                        leftInPattern <- newSTRef (toNumber firstDash)

                        -- Here we keep track of whether we're drawing or not drawing.
                        -- We start by drawing.
                        drawing <- newSTRef true

                        -- And, we'll want to track where we are
                        position <- newSTRef firstPoint

                        -- First, move to our first point
                        Canvas.moveTo ctx firstPoint.x firstPoint.y

                        let
                            -- If we're closed, we add the first point to those remaining
                            points =
                                if closed
                                    then snoc remainingPoints firstPoint
                                    else remainingPoints

                        -- Now, we iterate over the points
                        for_ points \destination ->
                            untilE do
                                currentPosition <- readSTRef position
                                currentlyDrawing <- readSTRef drawing
                                currentSegment <- readSTRef leftInPattern

                                let
                                    dx = destination.x - currentPosition.x
                                    dy = destination.y - currentPosition.y
                                    distance = sqrt ((dx * dx) + (dy * dy))
                                    operation = if currentlyDrawing then Canvas.lineTo else Canvas.moveTo

                                if distance < currentSegment
                                    then do
                                        -- Aha, we'll complete this point with the current
                                        -- segment. So, first we draw or move, to our
                                        -- destination.
                                        operation ctx destination.x destination.y

                                        -- Now, we'll remain with our current segment ... but
                                        -- we've used some of it, so we decrement
                                        writeSTRef leftInPattern (currentSegment - distance)

                                        -- And record our new position
                                        writeSTRef position destination

                                        -- And, we tell the untilE that we're done with this point,
                                        -- so move to the next destination
                                        pure true

                                    else do
                                        -- We've got more distance to travel than our current segment
                                        -- length. So, first we need to calculate what position we'll
                                        -- end up with for this segment.
                                        let
                                            nextPosition =
                                                { x: currentPosition.x + (dx * currentSegment / distance)
                                                , y: currentPosition.y + (dy * currentSegment / distance)
                                                }

                                        -- So, actually do the operation
                                        operation ctx nextPosition.x nextPosition.y

                                        -- And record our new position
                                        writeSTRef position nextPosition

                                        -- Now, we've used up this segment, so we need to flip our
                                        -- drawing state.
                                        writeSTRef drawing (not currentlyDrawing)

                                        -- And get the next pattern
                                        currentPattern <- readSTRef pattern

                                        -- We go down, and if at end back to first
                                        let nextPattern = fromMaybe firstPattern (down currentPattern)

                                        writeSTRef pattern nextPattern
                                        writeSTRef leftInPattern (toNumber (extract nextPattern))

                                        -- And, tell the untilE that we're not done with this destination yet
                                        pure false

                        -- We're done all the points ... just return the ctx
                        pure ctx

                _ ->
                    -- This the case where we have no dashing, so we can just trace the line
                    trace closed pointList ctx

            -- In either event, with or without dashing, we scale and stroke
            Canvas.scale {scaleX: 1.0, scaleY: (-1.0)} ctx
            Canvas.stroke ctx

        _ ->
            -- This is the case where we have an empty path, so there's nothing to do
            pure ctx


drawLine :: ∀ e. LineStyle -> Boolean -> List Point -> Context2D -> Eff (canvas :: Canvas | e) Context2D
drawLine style closed points =
    setStrokeStyle style >=> line style closed points


{-
	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(redo, ctx, style);
		ctx.scale(1, -1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0, -2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w / 2,
			destY = -h / 2,
			destW = w,
			destH = h;

		ctx.scale(1, -1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta % (Math.PI * 2));
		}
		if (scale !== 1)
		{
			ctx.scale(scale, scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta;
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}
 -}

str :: Number -> String
str n =
    if n < 0.00001 && n > (-0.00001)
        then "0"
        else show n

{-
	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i]._0.form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i]._0;
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}
-}

foreign import devicePixelRatio :: ∀ e. Window -> Eff (dom :: DOM | e) Number

makeCanvas :: ∀ e. Int -> Int -> Eff (dom :: DOM | e) DOM.Element
makeCanvas w h = do
    canvas <- createNode "canvas"

    setStyle "width" (show w <> "px") canvas
    setStyle "height" (show h <> "px") canvas
    setStyle "display" "block" canvas
    setStyle "position" "absolute" canvas

    ratio <- window >>= devicePixelRatio
    setAttribute "width" (show $ toNumber w * ratio) canvas
    setAttribute "height" (show $ toNumber h * ratio) canvas

    pure canvas

{-
	function nodeStepper(w, h, div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w, h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem._0.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w, h, div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w, h, forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w: w, h: h, forms: forms}
		});
	}
-}
