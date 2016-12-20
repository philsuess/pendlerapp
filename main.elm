import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Date exposing (..)
import Json.Decode as Json

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- model

type alias Member = {
  id: Int,
  name: String
}

type alias Ride = {
  id: Int,
  driver: Member,
  passengers: List Member
}

type alias Model = {
  date: Date,
  groupMembers: List Member,
  rides: List Ride
}

getNewRideId: Model -> Int
getNewRideId model =
  List.length model.rides

isValidMemberId: Model -> Int -> Bool
isValidMemberId model memberId =
  case getMaybeMemberById model memberId of
    Nothing -> False
    Just member -> True

getMaybeMemberById: Model -> Int -> Maybe Member
getMaybeMemberById model memberId =
  List.filter (\member -> member.id == memberId) model.groupMembers |> List.head

getMemberById: Model -> Int -> Member
getMemberById model memberId =
  case getMaybeMemberById model memberId of
    Nothing -> Member -1 "invalid member"
    Just member -> member

getMaybeRideById: Model -> Int -> Maybe Ride
getMaybeRideById model rideId =
  List.filter (\ride -> ride.id == rideId) model.rides |> List.head

getRideById: Model -> Int -> Ride
getRideById model rideId =
  case getMaybeRideById model rideId of
    Nothing -> Ride -1 (Member -1 "invalid ride") []
    Just ride -> ride

addPassengerToRide: Model -> Ride -> Member -> Model
addPassengerToRide model ride passenger =
  {
    model |
      rides = (model.rides ++ [
        (Ride (getNewRideId model) (getMemberById model passenger.id) [])
      ] )
  }

init: (Model, Cmd Msg)
init =
  (Model
    (Date.fromString "2016/12/18" |> Result.withDefault (Date.fromTime 0))
    [ (Member 0 "Phil"), (Member 1 "Jojo"), (Member 2 "Bernhard") ]
    [],
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
      let newIdAsInt = Result.withDefault -1 (String.toInt newDriverId) in
      if isValidMemberId model newIdAsInt then
        ( { model |
            rides = (model.rides ++ [
              (Ride (getNewRideId model) (getMemberById model newIdAsInt) [])
            ] )
          }, Cmd.none)
      else
        (model, Cmd.none)

    NewPassenger rideId newPassengerId ->
      let newIdAsInt = Result.withDefault -1 (String.toInt newPassengerId) in
      (addPassengerToRide model (getRideById model rideId) (getMemberById model newIdAsInt), Cmd.none)
      -- if isValidMemberId model newIdAsInt then
      --   -- ADD A PASSENGER TO THE RIDE HERE
      --   ( { model |
      --       rides = (model.rides ++ [
      --         (Ride (getNewRideId model) (getMemberById model newIdAsInt) [])
      --       ] )
      --     }, Cmd.none)
      -- else
      --   (model, Cmd.none)

-- views

view : Model -> Html Msg
view model =
  div [] [
    div [] [ viewDate model.date ],
    div [] [ viewConstellations model ]
  ]

-- drives

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
      tbody [] (List.reverse (viewNewRide model :: viewRides model))
    ]
  ]

-- rides

viewRides: Model -> List (Html Msg)
viewRides model =
  if List.isEmpty model.rides then
    [
      -- tr [] [
      --   td [] [ text "No rides selected yet" ],
      --   td [] [ text "no driver, no passengers..." ]
      -- ]
    ]
  else
    (List.map (\ride -> viewRide model ride) model.rides)

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
        tr [] (List.reverse ((viewNewPassenger model ride) :: (viewPassengers model ride)))
      ]
    ]
  ]

viewPassengers: Model -> Ride -> List (Html Msg)
viewPassengers model ride =
  (List.map viewPassenger ride.passengers)

viewPassenger: Member -> Html Msg
viewPassenger passenger =
  td [] [ text passenger.name ]

viewNewPassenger: Model -> Ride -> Html Msg
viewNewPassenger model ride =
  td [] [ select [ onSelectMember (NewPassenger ride.id) ] (newPassengerOptionList model.groupMembers) ]

newPassengerOptionList: List Member -> List (Html Msg)
newPassengerOptionList listMembers =
  (option [ value "-1" ] [ text "Add a passenger" ]) :: (List.map memberToOption listMembers)

viewNewRide: Model -> Html Msg
viewNewRide model =
  tr [] [
    td [] [ select [ onSelectMember NewDriver ] (newRideOptionList model.groupMembers) ],
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
