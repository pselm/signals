## Module Elm.VirtualDom

API to the core diffing algorithm. Can serve as a foundation for libraries
that expose more helper functions for HTML or SVG.

#### `Node`

``` purescript
data Node msg
```

An immutable chunk of data representing a DOM node. This can be HTML or SVG.

##### Instances
``` purescript
Functor Node
Renderable (Node msg)
```

#### `node`

``` purescript
node :: forall msg. String -> List (Property msg) -> List (Node msg) -> Node msg
```

Create a DOM node with a tag name, a list of HTML properties that can
include styles and event listeners, a list of CSS properties like `color`, and
a list of child nodes.

    import Json.Encode as Json

    hello :: Node msg
    hello =
      node "div" [] [ text "Hello!" ]

    greeting :: Node msg
    greeting =
      node "div"
        [ property "id" (Json.string "greeting") ]
        [ text "Hello!" ]

#### `keyedNode`

``` purescript
keyedNode :: forall msg. String -> List (Property msg) -> List (Tuple String (Node msg)) -> Node msg
```

Works just like `node`, but you add a unique identifier to each child
node. You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.

#### `fromRenderable`

``` purescript
fromRenderable :: forall a msg. Renderable a => a -> Node msg
```

Create a `Node` from anything that has a `Renderable` instance.

#### `text`

``` purescript
text :: forall msg. String -> Node msg
```

Just put plain text in the DOM. It will escape the string so that it appears
exactly as you specify.

    text "Hello World!"

#### `map`

``` purescript
map :: forall sub msg. (sub -> msg) -> Node sub -> Node msg
```

This function is useful when nesting components with [the Elm
Architecture](https://github.com/evancz/elm-architecture-tutorial/). It lets
you transform the messages produced by a subtree.

Say you have a node named `button` that produces `()` values when it is
clicked. To get your model updating properly, you will probably want to tag
this `()` value like this:

    type Msg = Click | ...

    update msg model =
      case msg of
        Click ->
          ...

    view model =
      map (\_ -> Click) button

So now all the events produced by `button` will be transformed to be of type
`Msg` so they can be handled by your update function!

#### `Property`

``` purescript
data Property msg
```

When using HTML and JS, there are two ways to specify parts of a DOM node.

  1. Attributes &mdash; You can set things in HTML itself. So the `class`
     in `<div class="greeting"></div>` is called an *attribute*.

  2. Properties &mdash; You can also set things in JS. So the `className`
     in `div.className = 'greeting'` is called a *property*.

So the `class` attribute corresponds to the `className` property. At first
glance, perhaps this distinction is defensible, but it gets much crazier.
*There is not always a one-to-one mapping between attributes and properties!*
Yes, that is a true fact. Sometimes an attribute exists, but there is no
corresponding property. Sometimes changing an attribute does not change the
underlying property. For example, as of this writing, the `webkit-playsinline`
attribute can be used in HTML, but there is no corresponding property!

#### `property`

``` purescript
property :: forall msg. String -> Value -> Property msg
```

Create arbitrary *properties*.

    import JavaScript.Encode as Json

    greeting : Html
    greeting =
        node "div" [ property "className" (Json.string "greeting") ] [
          text "Hello!"
        ]

Notice that you must give the *property* name, so we use `className` as it
would be in JavaScript, not `class` as it would appear in HTML.

#### `attribute`

``` purescript
attribute :: forall msg. String -> String -> Property msg
```

Create arbitrary HTML *attributes*. Maps onto JavaScript’s `setAttribute`
function under the hood.

    greeting : Html
    greeting =
        node "div" [ attribute "class" "greeting" ] [
          text "Hello!"
        ]

Notice that you must give the *attribute* name, so we use `class` as it would
be in HTML, not `className` as it would appear in JS.

#### `attributeNS`

``` purescript
attributeNS :: forall msg. String -> String -> String -> Property msg
```

Would you believe that there is another way to do this?! This corresponds
to JavaScript's `setAttributeNS` function under the hood. It is doing pretty
much the same thing as `attribute` but you are able to have "namespaced"
attributes. This is used in some SVG stuff at least.

* Note that the first argument is the namespace, the second the label, and
third the value. *

#### `style`

``` purescript
style :: forall msg. List (Tuple String String) -> Property msg
```

Specify a list of styles.

    myStyle :: Property msg
    myStyle =
      style
        [ Tuple "backgroundColor" "red"
        , Tuple "height" "90px"
        , Tuple "width" "100%"
        ]

    greeting :: Node msg
    greeting =
      node "div" [ myStyle ] [ text "Hello!" ]

#### `on`

``` purescript
on :: forall msg. String -> Decoder msg -> Property msg
```

Create a custom event listener.

    import Json.Decode as Json

    onClick : msg -> Property msg
    onClick msg =
      on "click" (Json.succeed msg)

You first specify the name of the event in the same format as with JavaScript’s
`addEventListener`. Next you give a JSON decoder, which lets you pull
information out of the event object. If the decoder succeeds, it will produce
a message and route it to your `update` function.

#### `onWithOptions`

``` purescript
onWithOptions :: forall msg. String -> Options -> Decoder msg -> Property msg
```

Same as `on` but you can set a few options.

#### `Options`

``` purescript
type Options = { stopPropagation :: Bool, preventDefault :: Bool }
```

Options for an event listener. If `stopPropagation` is true, it means the
event stops traveling through the DOM so it will not trigger any other event
listeners. If `preventDefault` is true, any built-in browser behavior related
to the event is prevented. For example, this is used with touch events when you
want to treat them as gestures of your own, not as scrolls.

#### `defaultOptions`

``` purescript
defaultOptions :: Options
```

Everything is `False` by default.

    defaultOptions =
        { stopPropagation = False
        , preventDefault = False
        }

#### `lazy`

``` purescript
lazy :: forall a msg. Eq a => (a -> Node msg) -> a -> Node msg
```

A performance optimization that delays the building of virtual DOM nodes.

Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
it. Calling `(lazy view model)` delays the call until later. During diffing, we
can check to see if `model` is referentially equal to the previous value used,
and if so, we just stop. No need to build up the tree structure and diff it,
we know if the input to `view` is the same, the output must be the same!

#### `lazy2`

``` purescript
lazy2 :: forall a b msg. (Eq a, Eq b) => (a -> b -> Node msg) -> a -> b -> Node msg
```

Same as `lazy` but checks on two arguments.

#### `lazy3`

``` purescript
lazy3 :: forall a b c msg. (Eq a, Eq b, Eq c) => (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
```

Same as `lazy` but checks on three arguments.


