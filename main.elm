import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
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
  passengers: Dict MemberId Member,
  editMode: Bool
}

invalidRide = Ride -1 invalidMember Dict.empty False

type alias Model = {
  date: Date,
  groupMembers: Dict MemberId Member,
  rides: Dict RideId Ride
}

getNewRideId: Model -> RideId
getNewRideId model = Dict.size model.rides

newRide: Model -> MemberId -> Ride
newRide model driverId = (Ride (getNewRideId model) (getMemberById model driverId) Dict.empty False)

convertToMemberId: String -> MemberId
convertToMemberId idAsString = Result.withDefault -1 (String.toInt idAsString)

isValidMemberId: Model -> MemberId -> Bool
isValidMemberId model memberId = Dict.member memberId model.groupMembers

isValidRideId: Model -> RideId -> Bool
isValidRideId model rideId = Dict.member rideId model.rides

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

updateRideDriverInModel: Model -> RideId -> MemberId -> Model
updateRideDriverInModel model rideId newDriverId =
  if isValidMemberId model newDriverId then
    let newDriver = getMemberById model newDriverId in
      { model | rides = Dict.update rideId (\ride -> updateRideDriver ride newDriver) model.rides }
  else
    model

updateRideDriver: Maybe Ride -> Member -> Maybe Ride
updateRideDriver ride newDriver =
  case ride of
    Nothing -> Just invalidRide
    Just ride -> Just { ride | driver = newDriver }

toggleRideEditModeInModel: Model -> RideId -> Model
toggleRideEditModeInModel model rideId =
  { model | rides = Dict.update rideId (\ride -> toggleRideEditMode ride) model.rides }

toggleRideEditMode: Maybe Ride -> Maybe Ride
toggleRideEditMode ride =
  case ride of
    Nothing -> Just invalidRide
    Just ride -> Just { ride | editMode = not ride.editMode }

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

removePassengerFromRideInModel: Model -> RideId -> MemberId -> Model
removePassengerFromRideInModel model rideId passengerId =
  if isValidMemberId model passengerId && isValidRideId model rideId then
    { model | rides = Dict.update rideId (\ride -> removePassengerFromRide ride passengerId) model.rides }
  else
    model

removePassengerFromRide: Maybe Ride -> MemberId -> Maybe Ride
removePassengerFromRide ride passengerId =
  case ride of
    Nothing -> Just invalidRide
    Just ride -> Just { ride | passengers = Dict.remove passengerId ride.passengers }

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
  | UpdateDriver RideId String
  | NewPassenger RideId String -- arguemnts are rideId and passenger id as string
  | RemovePassenger RideId MemberId
  | EditRide RideId -- argument is rideId

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewDriver newDriverIdAsString ->
      (addNewRide model (convertToMemberId newDriverIdAsString), Cmd.none)

    UpdateDriver rideId newDriverIdAsString ->
      ((updateRideDriverInModel model rideId (convertToMemberId newDriverIdAsString)), Cmd.none)

    NewPassenger rideId newPassengerId ->
      (addPassengerToRideInModel model rideId (getMemberById model (convertToMemberId newPassengerId)), Cmd.none)

    RemovePassenger rideId passengerId ->
      ((removePassengerFromRideInModel model rideId passengerId), Cmd.none)

    EditRide rideId -> ((toggleRideEditModeInModel model rideId), Cmd.none)


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
          th [] [ text "Passengers" ],
          th [] [ text "Options" ]
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
      --   td [] [ text "no driver, no passengers..." ],
      --   td [] [ text "no options needed for no ride" ]
      -- ]
    ]
  else
    (List.map (\ride -> viewRide model ride) (Dict.values model.rides))

viewRide: Model -> Ride -> Html Msg
viewRide model ride =
  tr [] [
    viewRideDriver model ride,
    td [] (viewPassengersTable model ride),
    td [] [ button [ onClick (EditRide ride.id) ] [ text "Toggle edit mode" ] ]
  ]

viewRideDriver: Model -> Ride -> Html Msg
viewRideDriver model ride =
  if ride.editMode then
    td [] [ selectDriver model (UpdateDriver ride.id) ride.driver ]
  else
    td [] [ text ride.driver.name ]

selectDriver: Model -> (String -> Msg) -> Member -> Html Msg
selectDriver model msg currentDriver =
  if isValidMemberId model currentDriver.id then
    select [ onSelectMember msg ]
      (List.append
        (driversOptionList (Dict.values (getMembersNotYetParticipatingToday model)) ("Choose driver (currently " ++ currentDriver.name ++")"))
        [(option [ value (toString currentDriver.id), selected True ] [ text currentDriver.name ])]
      )
  else
    select [ onSelectMember msg ]
      (driversOptionList (Dict.values (getMembersNotYetParticipatingToday model)) "Add new driver" )

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
  (List.map (\passenger -> viewPassenger passenger ride.id) (Dict.values ride.passengers))

viewPassenger: Member -> RideId -> Html Msg
viewPassenger passenger rideId =
  td [] [
    div [] [
      text passenger.name,
      br [] [],
      button [ onClick (RemovePassenger rideId passenger.id) ] [ text "Remove" ]
    ]
  ]

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
      td [] [ selectDriver model NewDriver invalidMember ],
      td [] [],
      td [] []
    ]

driversOptionList: List Member -> String -> List (Html Msg)
driversOptionList listMembers defaultEntry =
  (option [ value "-1" ] [ text defaultEntry ]) :: (List.map memberToOption listMembers)

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
