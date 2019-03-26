import Browser
import File exposing (File)
import Html as H exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..) 
import Task 
import Process
import Parser exposing (..)
import Dict exposing (Dict)
--import Canvas exposing (..) -- Had trouble installing this...? Would probably be better than using Svg
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Json.Decode as D
import Json.Encode as E

import XmlParser exposing (..)



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

-- These might not be needed
type EditorMode
    = RoadEditing
    | ControllerEditing
    | JunctionEditing
    | JunctionGroupEditing
    | HeaderEditing

type ProcessingState 
    = Waiting
    | FileLoaded
    | XMLParsing
    | ParsingError
    | XMLCorrectlyParsed


type alias Model =
  { state : ProcessingState
  , xml : String -- TODO: remove this as soon as you don't need it anymore
  , active : Maybe RoadElement
  , elements : Dict Int RoadElement
--  , roads : List (Int, String) -- the id and name of each road. Should be exended to junctions, junctionsgroups and other top level elements
  }



init : () -> (Model, Cmd Msg)
init _ =
  ({state = Waiting, xml = "", active = Nothing, elements = Dict.empty}, Cmd.none)



-- UPDATE


type Msg
  = GotFiles (List File)
  | ReadFile (String)
  | ParsedXML (Dict Int RoadElement)
  | ChangeActive Int
  | ChangeValue  -- TODO make this actually change the value in the dict


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFiles files ->
      ({model | state = FileLoaded, xml = "File selected. Reading file..."}, readFiles files )
    ReadFile result -> ({model | state = XMLParsing, xml = "XML file read. Parsing..."}, concurrParse result) 
    ParsedXML result -> ({model | state = XMLCorrectlyParsed, xml = "XML parsed.", elements = result}, Cmd.none) 
    ChangeActive id -> ({model | active = Dict.get id model.elements}, Cmd.none)
    ChangeValue -> ({model | xml="Value change event registred!"}, Cmd.none)

readFiles : List File -> Cmd Msg
readFiles files = case (List.head files) of
    Nothing -> Cmd.none 
    Just file -> Task.perform ReadFile (File.toString file) 


-- TODO: change task.perform to attempt and handle parsing error here
concurrParse : String -> Cmd Msg
concurrParse s = Task.perform identity (Task.succeed (ParsedXML (parseRoadXML s))) 

-- All this probably isn't needed now. Rather go through the elements created by parseRoadXML for the ids and names
-- parseTopXML : String -> List (Int, String)
-- parseTopXML filestring = case (XmlParser.parse filestring) of
--     Ok xml -> handleTopXML xml
--     Err l -> [] --Parsing Error!

-- handleTopXML : Xml -> List (Int, String)
-- handleTopXML xml = case (xml.root) of
--     Element s l ln -> parseTopLevel ln
--     Text s -> []
        
parseRoadXML : String -> Dict Int RoadElement
parseRoadXML filestring = case (XmlParser.parse filestring) of 
    Ok xml -> handleRoadXML xml 
    Err l -> Dict.empty

handleRoadXML : Xml -> Dict Int RoadElement
handleRoadXML xml = case (xml.root) of 
    Element s l ln -> parseRoadElements ln 
    Text s -> Dict.empty

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
        input
            [ HA.type_ "file"
            , multiple True
            , on "change" (D.map GotFiles filesDecoder)
        ] []
        , div [HA.class "main"] [
              div [HA.class "roads"] (renderRoadElements model.elements)
            , div [HA.class "active"] (renderActive model.active)
            , div [HA.class "gfx"] [(renderRoadsGraphic model.elements model.active)]
        ]
    ]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)

renderRoadElements : Dict Int RoadElement -> List (Html Msg)
renderRoadElements roads = List.map renderRoad (Dict.values roads)

renderRoad : RoadElement -> Html Msg
renderRoad element = button [onClick (ChangeActive element.id), HA.class "road"] [div [] [H.text (String.fromInt element.id)], div [] [H.text (element.name)]]

renderActive : Maybe RoadElement -> List (Html Msg)
renderActive re = case re of
    Just r -> [div [] 
                    [
                        div [] [(H.text "Length"), viewInput "text" (String.fromFloat r.length) (String.fromFloat r.length) (\_ -> ChangeValue) ],
                        div [] [(H.text "Junction"), viewInput "text" (String.fromInt r.junction) (String.fromInt r.junction) (\_ -> ChangeValue) ],
                        div [] [(H.text "Links"), renderLinks r.link],
                        div [] [(H.text "Geometry (planView)"), renderGeometryForm r.planView],
                        div [] [(H.text "Lanes"), renderLanes r.lanes]
                    ]
              ]
    Nothing -> [H.text "Active not set :("]

renderLinks : Maybe RoadLink -> Html Msg
renderLinks l = case l of 
    Nothing -> div [] [(H.text "No links given.")]
    Just rl -> div [] [div [] [(H.text "Predecessor: "), (H.text (cessorToString rl.predecessor))], div [] [(H.text "Successor: "), (H.text (cessorToString rl.successor))]]

renderGeometryForm : List RoadGeometry -> Html Msg
renderGeometryForm lrg = div [] (List.map renderGeometryElemForm lrg)

renderGeometryElemForm : RoadGeometry -> Html Msg
renderGeometryElemForm rg = div [] 
    [ div [] [H.text "s: ", H.text (String.fromFloat rg.s)]
    , div [] [H.text "x: ", H.text (String.fromFloat rg.x)]
    , div [] [H.text "y: ", H.text (String.fromFloat rg.y)]
    , div [] [H.text "hdg: ", H.text (String.fromFloat rg.hdg)]
    , div [] [H.text "Length: ", H.text (String.fromFloat rg.length)]
    , div [] [H.text "Elem: ", H.text "Line (fix this later)"]
    ]

viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput t p v toMsg =
  input [ HA.type_ t, placeholder p, value v, onInput toMsg ] []

renderRoadsGraphic : Dict Int RoadElement -> Maybe RoadElement -> Html Msg
renderRoadsGraphic elements active = case Dict.isEmpty elements of 
    False -> svg 
        [ SA.width "700"
        , SA.height "700"
        , viewBox "0 0 700 700"
        ]
        (List.map renderRoadGraphic (Dict.values elements))
    True -> svg [] []

renderRoadGraphic : RoadElement -> Svg Msg
renderRoadGraphic re = svg [] (List.map renderRoadGeometry re.planView)

renderRoadGeometry : RoadGeometry -> Svg Msg
renderRoadGeometry rg = case rg.geometryElem of 
    Line -> line [
        x1 (String.fromFloat (normalize rg.x rg.y)), 
        y1 (String.fromFloat (normalize rg.y rg.x)), 
        x2 (String.fromFloat (normalize ((rg.x + rg.length * (cos rg.hdg))) ((rg.y + rg.length * (sin rg.hdg))) )), 
        y2 (String.fromFloat (normalize ((rg.y + rg.length * (sin rg.hdg))) ((rg.x + rg.length * (cos rg.hdg))))), 
        stroke "black"] []
    Spiral sa -> svg [] []

normalize : Float -> Float -> Float
normalize n1 n2 = (700 * n1) / (sqrt ((n1 ^ 2) + (n2 ^ 2)))

renderLanes : RoadLanes -> Html Msg 
renderLanes rl = div [] [H.text "even more WOOP!"]

-- OpenDrive spec 

-- TODO: Make Model have an OpenDrive tree in it instead and make view render these elements correctly

-- Not sure if this is needed
-- type OpenDriveElement 
--     = Road Int String
--     | Controller 
--     | Junction 
--     | JunctionGroup
--     | Station

-- TODO: Things not implemented yet: ElevationProfile, LateralProfile, 
type alias RoadElement = 
    { id : Int
    , name : String
    , length : Float
    , junction : Int
    , link : Maybe RoadLink
--    , type : List RoadType
    , planView : List RoadGeometry
    , lanes : RoadLanes
--    , objects : List RoadObject
    }

defaultRoadElement : RoadElement
defaultRoadElement = {id = -1, name = "-Road not named-", length = 0.0, junction = -1, link = Nothing, planView = [], lanes = defaultRoadLanes}

-- TODO: The RoadLink parts should probably all be separate objects
type alias RoadLink = 
    { predecessor : Maybe Cessor
    , successor : Maybe Cessor
    , neighbor : Maybe LinkNeighbor
    }

defaultRoadLink : RoadLink
defaultRoadLink = {predecessor = Nothing, successor = Nothing, neighbor = Nothing}

type alias Cessor =
    { elementType : CessorType
    , elementId : Int
    , contactPoint : CessorCPoint 
    }

defaultCessor : Cessor
defaultCessor = {elementType = Road, elementId = -1, contactPoint = Start}

cessorToString : Maybe Cessor -> String
cessorToString c = case c of
    Nothing -> "No link"
    Just cc -> String.fromInt cc.elementId

type CessorType 
    = Road 
    | Junction

type CessorCPoint 
    = Start 
    | End

type alias LinkNeighbor = 
    { side : NeighborSide
    , elementId : Int 
    , direction : NeighborDir 
    }

defaultNeighbor : LinkNeighbor
defaultNeighbor = {side = Left, elementId = -1, direction = Same}

type NeighborSide 
    = Left 
    | Right 

type NeighborDir 
    = Same 
    | Opposite

type alias RoadGeometry =
    { s : Float
    , x : Float 
    , y : Float
    , hdg : Float
    , length : Float
    , geometryElem : GeometryElement
    }

defaultRoadGeometry : RoadGeometry 
defaultRoadGeometry = 
    { s = 0.0
    , x = 0.0
    , y = 0.0
    , hdg = 0.0
    , length = 0.0
    , geometryElem = Line
    }

type GeometryElement 
    = Line
    | Spiral SpiralAttri

type alias SpiralAttri =
    { curvStart : Float
    , curvEnd : Float
    }


type alias RoadLanes = 
    { laneOffsets : List ODLaneOffset
    , laneSections : List ODLaneSection
    }

defaultRoadLanes : RoadLanes
defaultRoadLanes = {laneOffsets = [], laneSections = []}

type alias ODLaneOffset =
    { s : Float
    , a : Float
    , b : Float
    , c : Float
    , d : Float
    }

defaultODLaneOffset : ODLaneOffset 
defaultODLaneOffset = {s = 0.0, a = 0.0, b = 0.0, c = 0.0, d = 0.0}

type alias ODLaneSection = -- 1+ 
    { s : Float
    , singleSide : Bool
    , left : Maybe ODLane
    , center : Maybe ODLane
    , right : Maybe ODLane
    }

defaultODLaneSection : ODLaneSection  
defaultODLaneSection = {s = 0.0, singleSide = False, left = Nothing, center = Nothing, right = Nothing}

type alias ODLane = 
    { id : Int
    , ttype : String 
    , level : Bool 
    , link : Maybe RoadLink -- TODO This can't fully be a RoadLink in the end since it doesnt have a Neighbor field. But oh well have it as nothing until then.
    , width : ODLaneWidth
--    , border : List ODLaneBorder
    }

type alias ODLaneWidth = 
    { soffset : Float 
    , a : Float 
    , b : Float 
    , c : Float 
    , d : Float 
    }

--type alias ODLaneBorder = -- 1+
--    {}

correctRoot : String -> Bool
correctRoot s = (String.contains "OpenDRIVE" s) && ((String.length s) == 9)

-- All this is probably not needed anymore since it is better to just go through the dict created by the real parsing
--parseTopLevel : (List XmlParser.Node) -> (List (Int, String))
--parseTopLevel nodes = List.filterMap (parseTopLevelElements) nodes

-- parseTopLevelElements : XmlParser.Node -> Maybe RoadElement
-- parseTopLevelElements node = case node of
--     (Text _) -> Nothing
--     (Element elemType attr _) -> case elemType of
--         "road" -> let re = (parseRoadAttri (attr) defaultRoadElement) in
--             case (re.id /= -1) of 
--                 True -> Just (re.id, noNameCheck name)
--                 False -> Nothing
--         _ -> Nothing -- TODO: make this complete to handle junctions etc


-- TODO this should probably be remade into just a Int. This would be more efficient. Maybe... Though maybe this should get all the attribs
parseRoadAttri : List XmlParser.Attribute -> RoadElement -> RoadElement
parseRoadAttri attributes element = List.foldl (ra) element attributes

ra : XmlParser.Attribute -> RoadElement -> RoadElement
ra a el = case a.name of
    "id" -> {el | id = Maybe.withDefault -1 (String.toInt a.value)}
    "name" -> if (String.isEmpty a.value) then el else {el | name = a.value}
    "length" -> {el | length = Maybe.withDefault 0.0 (String.toFloat a.value)}
    _ -> el


-- TODO this is probably too complex. No need to "mapRoadElements" into Maybe. Why not just go through and then throw away things with id = -1. 
-- TODO this is kinda the new parseTopLevel so make it more generic to handle all the toplevel. Not that I have the foldl pattern just do it for everything.
-- With this, you should probably make it all a OpenDriveElement in Model.
parseRoadElements : (List XmlParser.Node) -> Dict Int RoadElement
parseRoadElements nodes = Dict.fromList (List.filterMap mapRoadElements nodes) 

mapRoadElements : XmlParser.Node -> Maybe (Int, RoadElement)
mapRoadElements node = case node of
    (Text _) -> Nothing
    (Element elemType attr children) -> case elemType of
        "road" -> let re = (parseRoadAttri attr defaultRoadElement) in
            case (re.id /= -1) of 
                True -> Just (re.id, parseRoadChildren children re)
                False -> Nothing
        _ -> Nothing -- TODO: make this complete to handle junctions etc

parseRoadChildren : List XmlParser.Node -> RoadElement -> RoadElement
parseRoadChildren l re= List.foldl (buildRoadElement) re l

buildRoadElement : XmlParser.Node -> RoadElement -> RoadElement
buildRoadElement n prev = case n of 
    Element name attribs children -> case name of 
        "link" -> {prev | link = Just (List.foldl (parseLink) (defaultRoadLink) children)}
        "planView" -> {prev | planView = List.map parsePlanView children}
        "lanes" -> {prev | lanes = List.foldl parseLanes (defaultRoadLanes) children}
        _ -> prev
    Text s -> prev

parseLink : XmlParser.Node -> RoadLink -> RoadLink
parseLink n prev = case n of 
    Element name attri _ -> case name of 
        "predecessor" -> {prev | predecessor = Just (List.foldl (parseCessor) defaultCessor attri)}
        "successor" -> {prev | successor = Just (List.foldl (parseCessor) defaultCessor attri)}
        "neighbor" -> {prev | neighbor = Just (List.foldl (parseNeighbor) defaultNeighbor attri)}
        _ -> prev
    Text _ -> prev

parseCessor : XmlParser.Attribute -> Cessor -> Cessor
parseCessor a prev = case a.name of
    "elementType" -> if a.value == "road" then {prev | elementType = Road} else {prev | elementType = Junction}
    "elementId" -> {prev | elementId = Maybe.withDefault (-1) (String.toInt a.value)} -- TODO you could filter out bad ones on this value if you really want
    "contactPoint" -> if a.value == "start" then {prev | contactPoint = Start} else {prev | contactPoint = End}
    _ -> prev

parseNeighbor : XmlParser.Attribute -> LinkNeighbor -> LinkNeighbor
parseNeighbor a prev = prev

parsePlanView : XmlParser.Node -> RoadGeometry
parsePlanView n = case n of 
    Element name attri children -> (List.foldl parseGeometryElements (List.foldl (parseGeometryAttri) defaultRoadGeometry attri) children)
    Text _ -> defaultRoadGeometry

parseGeometryAttri : XmlParser.Attribute -> RoadGeometry -> RoadGeometry
parseGeometryAttri a prev = case a.name of 
    "s"      -> {prev | s = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "x"      -> {prev | x = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "y"      -> {prev | y = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "hdg"    -> {prev | hdg = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "length" -> {prev | length = Maybe.withDefault (0.0) (String.toFloat a.value)}
    _ -> prev

parseGeometryElements : XmlParser.Node -> RoadGeometry -> RoadGeometry
parseGeometryElements n prev = case n of 
    Element name attri children -> case name of 
        "line" -> {prev | geometryElem = Line} -- TODO add all other kinds of geometry elements (spiral, arc ...)
        _ -> prev 
    Text _ -> prev

parseLanes : XmlParser.Node -> RoadLanes -> RoadLanes
parseLanes n prev = case n of 
    Element name attri children -> case name of 
        "laneOffset"  -> {prev | laneOffsets = (List.foldl parseLaneOffset defaultODLaneOffset attri) :: prev.laneOffsets}
        "laneSection" -> {prev | laneSections = List.foldl parseLaneSection (List.foldl parseLaneSectionAttri defaultODLaneSection attri) children :: prev.laneSections}
        _ -> prev
    Text _ -> defaultRoadLanes

parseLaneOffset : XmlParser.Attribute -> ODLaneOffset -> ODLaneOffset 
parseLaneOffset a prev = case a.name of 
    "s" -> {prev | s = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "a" -> {prev | a = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "b" -> {prev | s = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "c" -> {prev | s = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "d" -> {prev | s = Maybe.withDefault (0.0) (String.toFloat a.value)}
    _ -> prev

parseLaneSection : XmlParser.Node -> ODLaneSection -> ODLaneSection 
parseLaneSection n prev = case n of 
    Element name attri children -> case name of 
        "left" -> prev 
        "center" -> prev 
        "right" -> prev 
        _ -> prev
    Text _ -> prev

parseLaneSectionAttri : XmlParser.Attribute -> ODLaneSection -> ODLaneSection  
parseLaneSectionAttri a prev = case a.name of 
    "s" -> {prev | s = Maybe.withDefault (0.0) (String.toFloat a.value)}
    "singleSide" -> if a.value == "true" then {prev | singleSide = True} else {prev | singleSide = False}
    _ -> prev


-- Make the Elements into XML again for download
-- encodeXML : OpenDriveElement -> String
-- encodeXML od = ""
