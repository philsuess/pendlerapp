import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Date exposing (..)
import Json.Decode as Json
import Dict exposing (Dict)

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- model

type alias MemberId = Int
type alias Member = {
  id: MemberId,
  name: String
}

invalidMember = Member -1 "invalid member"

type alias RideId = Int
type alias Ride = {
  id: RideId,
  driver: Member,
  passengers: Dict MemberId Member
}

invalidRide = Ride -1 invalidMember Dict.empty

type alias Model = {
  date: Date,
  groupMembers: Dict MemberId Member,
  rides: Dict RideId Ride
}

getNewRideId: Model -> RideId
getNewRideId model = Dict.size model.rides

newRide: Model -> MemberId -> Ride
newRide model driverId = (Ride (getNewRideId model) (getMemberById model driverId) Dict.empty)

convertToMemberId: String -> MemberId
convertToMemberId idAsString = Result.withDefault -1 (String.toInt idAsString)

isValidMemberId: Model -> MemberId -> Bool
isValidMemberId model memberId = Dict.member memberId model.groupMembers

getMemberById: Model -> MemberId -> Member
getMemberById model memberId =
  case Dict.get memberId model.groupMembers of
    Nothing -> invalidMember
    Just member -> member

getRideById: Model -> RideId -> Ride
getRideById model rideId =
  case Dict.get rideId model.rides of
    Nothing -> invalidRide
    Just ride -> ride

addNewRide: Model -> MemberId -> Model
addNewRide model driverId =
  if isValidMemberId model driverId then
    let ride = newRide model driverId in
      { model | rides = Dict.insert ride.id ride model.rides }
  else
    model

addPassengerToRideInModel: Model -> RideId -> Member -> Model
addPassengerToRideInModel model rideId passenger =
  if isValidMemberId model passenger.id then
    { model | rides = Dict.update rideId (\ride -> addPassengerToRide ride passenger) model.rides }
  else
    model

addPassengerToRide: Maybe Ride -> Member -> Maybe Ride
addPassengerToRide ride passenger =
  case ride of
    Nothing -> Just invalidRide
    Just ride -> Just { ride | passengers = (Dict.insert passenger.id passenger ride.passengers) }

getMembersNotYetParticipatingToday: Model -> Dict MemberId Member
getMembersNotYetParticipatingToday model =
  Dict.diff model.groupMembers (getParticipantsForToday model)

getParticipantsForToday: Model -> Dict MemberId Member
getParticipantsForToday model = Dict.union (getDriversForToday model) (getPassengersForToday model)

getDriversForToday: Model -> Dict MemberId Member
getDriversForToday model =
  -- loop over all rides:
  -- Dict.foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
  -- with
  -- b = (Dict MemberId Member)
  -- comparable = RideId
  -- v = Ride
  -- (comparable -> v -> b -> b) translates to
  -- ( ride id -> ride -> (Dict MemberId Member) -> (Dict MemberId Member))  )
  -- the accumulator gets inserted the drivers with the correct ids as key
  Dict.foldl (\rideId ride -> (\accumulator -> (Dict.insert ride.driver.id ride.driver accumulator))) Dict.empty model.rides

getPassengersForToday: Model -> Dict MemberId Member
getPassengersForToday model =
  -- loop over all rides:
  -- Dict.foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
  -- with
  -- b = (Dict MemberId Member)
  -- comparable = RideId
  -- v = Ride
  -- (comparable -> v -> b -> b) translates to
  -- ( ride id -> ride -> (Dict MemberId Member) -> (Dict MemberId Member))  )
  -- the accumulator just gathers all passenger dicts using Dict.union
  Dict.foldl (\rideId ride -> (\accumulator -> (Dict.union accumulator ride.passengers))) Dict.empty model.rides


-- init

getGroupMembers: Dict MemberId Member
getGroupMembers =
  Dict.empty
    |> Dict.insert 0 (Member 0 "Phil")
    |> Dict.insert 1 (Member 1 "Jojo")
    |> Dict.insert 2 (Member 2 "Bernhard")

init: (Model, Cmd Msg)
init =
  (Model
    (Date.fromString "2016/12/18" |> Result.withDefault (Date.fromTime 0))
    getGroupMembers
    Dict.empty,
  Cmd.none)


-- subs

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- updates

type Msg =
  NewDriver String -- argument is id as a string
  | NewPassenger Int String -- arguemnts are rideId and passenger id as string

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewDriver newDriverId ->
      (addNewRide model (convertToMemberId newDriverId), Cmd.none)

    NewPassenger rideId newPassengerId ->
      (addPassengerToRideInModel model rideId (getMemberById model (convertToMemberId newPassengerId)), Cmd.none)

-- views

view : Model -> Html Msg
view model =
  div [] [
    div [] [ viewDate model.date ],
    div [] [ viewConstellations model ],
    div [] [ viewSummary model ]
  ]

-- drives

viewSummary: Model -> Html Msg
viewSummary model =
  div [] [
    h1 [] [ text "Today's drivers are" ],
    div [] (List.map (\member -> div [] [ text member.name ]) (Dict.values (getDriversForToday model))),
    h1 [] [ text "Today's driver ids are" ],
    div [] (List.map (\id -> div [] [ text (toString id) ]) (Dict.keys (getDriversForToday model))),
    h1 [] [ text "Today's passengers are" ],
    div [] (List.map (\member -> div [] [ text member.name ]) (Dict.values (getPassengersForToday model))),
    h1 [] [ text "Today's participants are" ],
    div [] (List.map (\member -> div [] [ text member.name ]) (Dict.values (getParticipantsForToday model))),
    h1 [] [ text "Not participating today are" ],
    div [] (List.map (\member -> div [] [ text member.name ]) (Dict.values (getMembersNotYetParticipatingToday model)))
  ]

viewConstellations: Model -> Html Msg
viewConstellations model =
  div [] [
    table [] [
      thead [] [
        tr [] [
          th [] [ text "Driver" ],
          th [] [ text "Passengers" ]
        ]
      ],
      tbody [] ((viewRides model) ++ [ viewNewRide model ])
    ]
  ]

-- rides

viewRides: Model -> List (Html Msg)
viewRides model =
  if Dict.isEmpty model.rides then
    [
      -- tr [] [
      --   td [] [ text "No rides selected yet" ],
      --   td [] [ text "no driver, no passengers..." ]
      -- ]
    ]
  else
    (List.map (\ride -> viewRide model ride) (Dict.values model.rides))

viewRide: Model -> Ride -> Html Msg
viewRide model ride =
  tr [] [
    td [] [ text ride.driver.name ],
    td [] (viewPassengersTable model ride)
  ]

viewPassengersTable: Model -> Ride -> List (Html Msg)
viewPassengersTable model ride =
  [
    table [] [
      tbody [] [
        tr [] ((viewPassengers model ride) ++ [ viewNewPassenger model ride ])
      ]
    ]
  ]

viewPassengers: Model -> Ride -> List (Html Msg)
viewPassengers model ride =
  (List.map viewPassenger (Dict.values ride.passengers))

viewPassenger: Member -> Html Msg
viewPassenger passenger =
  td [] [ text passenger.name ]

viewNewPassenger: Model -> Ride -> Html Msg
viewNewPassenger model ride =
  let remainingMembers = getMembersNotYetParticipatingToday model in
  if (Dict.isEmpty remainingMembers) then
    td [] []
  else
    td [] [
      select [ onSelectMember (NewPassenger ride.id) ]
        (newPassengerOptionList (Dict.values (getMembersNotYetParticipatingToday model)))
    ]

newPassengerOptionList: List Member -> List (Html Msg)
newPassengerOptionList listMembers =
  (option [ value "-1" ] [ text "Add a passenger" ]) :: (List.map memberToOption listMembers)

viewNewRide: Model -> Html Msg
viewNewRide model =
  let remainingMembers = getMembersNotYetParticipatingToday model in
  if (Dict.isEmpty remainingMembers) then
    tr [] []
  else
    tr [] [
      td [] [
        select [ onSelectMember NewDriver ]
          (newRideOptionList (Dict.values (getMembersNotYetParticipatingToday model)))
      ],
      td [] []
    ]

newRideOptionList: List Member -> List (Html Msg)
newRideOptionList listMembers =
  (option [ value "-1" ] [ text "Add a driver" ]) :: (List.map memberToOption listMembers)

onSelectMember: (String -> msg) -> Html.Attribute msg
onSelectMember msg =
  on "change" (Json.map msg targetValue)

memberToOption: Member -> Html Msg
memberToOption member =
  option [ value (toString member.id) ] [ text member.name ]

-- date view

viewDate: Date -> Html Msg
viewDate date =
  h1 [] [ text (dateToString date) ]

dateToString: Date -> String
dateToString date = toString (month date) ++ " " ++ toString (day date) ++ ", " ++ toString (year date)
