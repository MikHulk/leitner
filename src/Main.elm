port module Main exposing (..)

import Browser
import Element as E
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as F
import Element.Input as I
import Html exposing (Html, node)
import Html.Attributes exposing (attribute)
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init cards =
    ( { now = Time.millisToPosix 0
      , cards = List.map parseCard cards
      , view = HomeView
      , newQuestion = { question = "", answer = "", contentType = Raw }
      , answerRevealed = False
      }
    , Task.perform NewTime Time.now
    )


type alias Flags =
    List CardFromStorage


type alias CardFromStorage =
    { question : String
    , correctAnswer : { contentType : String, value : String }
    , rank : Int
    , nextReviewAt : Int
    }


port saveCards : List CardFromStorage -> Cmd msg


type Msg
    = NewTime Time.Posix
    | UserInteracts UserAction


type UserAction
    = ChooseAddQuestion
    | ChooseAnswerQuestion
    | ChooseManageQuestions
    | WantGoHome
    | TypeNewQuestion String
    | TypeNewAnswer String
    | ChooseAnswerContentType Content
    | SubmitNewCard
    | RevealAnswer
    | ValidateAnswer Bool
    | RemoveCard Int


type alias Card =
    { question : String
    , correctAnswer :
        { contentType : Content
        , value : String
        }
    , rank : Int
    , nextReviewAt : Time.Posix
    }


type Content
    = Raw
    | MathFormula


type alias NewQuestion =
    { question : String
    , answer : String
    , contentType : Content
    }


type View
    = HomeView
    | AddQuestionView
    | AnswerQuestionView
    | ManageQuestionsView


type alias Model =
    { now : Time.Posix
    , cards : List Card
    , view : View
    , newQuestion : NewQuestion
    , answerRevealed : Bool
    }


oneDay : Int
oneDay =
    864 * 10 ^ 5


fromMonth : Time.Month -> String
fromMonth month =
    case month of
        Time.Jan ->
            "jan"

        Time.Feb ->
            "feb"

        Time.Mar ->
            "mar"

        Time.Apr ->
            "apr"

        Time.May ->
            "may"

        Time.Jun ->
            "jun"

        Time.Jul ->
            "jul"

        Time.Aug ->
            "aug"

        Time.Sep ->
            "sep"

        Time.Oct ->
            "oct"

        Time.Nov ->
            "nov"

        Time.Dec ->
            "dec"


formatDate : Time.Posix -> String
formatDate time =
    String.concat
        [ String.fromInt <| Time.toDay Time.utc time
        , " "
        , fromMonth <| Time.toMonth Time.utc time
        , " "
        , String.fromInt <| Time.toYear Time.utc time
        ]


fibo : Int -> Int
fibo n =
    let
        go a b c =
            if c == 0 then
                a

            else
                go (a + b) a (c - 1)
    in
    go 1 1 n


processCard : Time.Posix -> Card -> Bool -> Card
processCard now card answerIsGood =
    let
        rank =
            if answerIsGood then
                if card.rank >= 12 then
                    12

                else
                    card.rank + 1

            else
                0

        timeOffset =
            oneDay * fibo rank

        nextDate =
            Time.millisToPosix <|
                Time.posixToMillis now
                    + timeOffset
    in
    { card | rank = rank, nextReviewAt = nextDate }


selectCard : Time.Posix -> List Card -> Maybe Card
selectCard now cards =
    case cards of
        card :: _ ->
            if Time.posixToMillis now > Time.posixToMillis card.nextReviewAt then
                Just card

            else
                Nothing

        [] ->
            Nothing


parseCard : CardFromStorage -> Card
parseCard card =
    { question = card.question
    , correctAnswer =
        { contentType =
            case card.correctAnswer.contentType of
                "MathFormula" ->
                    MathFormula

                _ ->
                    Raw
        , value = card.correctAnswer.value
        }
    , rank = card.rank
    , nextReviewAt = Time.millisToPosix card.nextReviewAt
    }


encodeCard : Card -> CardFromStorage
encodeCard card =
    { question = card.question
    , correctAnswer =
        { contentType =
            case card.correctAnswer.contentType of
                MathFormula ->
                    "MathFormula"

                _ ->
                    "Raw"
        , value = card.correctAnswer.value
        }
    , rank = card.rank
    , nextReviewAt = Time.posixToMillis card.nextReviewAt
    }


insertCard : List Card -> Card -> List Card
insertCard cards newCard =
    let
        go checked remain =
            case remain of
                [] ->
                    List.reverse (newCard :: checked)

                cur :: rem ->
                    let
                        millisCurrent =
                            Time.posixToMillis cur.nextReviewAt

                        millisNewCard =
                            Time.posixToMillis newCard.nextReviewAt
                    in
                    if millisCurrent > millisNewCard then
                        List.reverse checked ++ newCard :: cur :: rem

                    else
                        go (cur :: checked) rem
    in
    go [] cards


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime time ->
            ( { model | now = time }, Cmd.none )

        UserInteracts action ->
            userAction action model


userAction : UserAction -> Model -> ( Model, Cmd Msg )
userAction action model =
    case action of
        ChooseAddQuestion ->
            ( { model
                | view = AddQuestionView
                , newQuestion = { question = "", answer = "", contentType = Raw }
              }
            , Task.perform NewTime Time.now
            )

        ChooseAnswerQuestion ->
            ( { model | view = AnswerQuestionView, answerRevealed = False }
            , Task.perform NewTime Time.now
            )

        ChooseManageQuestions ->
            ( { model | view = ManageQuestionsView, answerRevealed = False }
            , Task.perform NewTime Time.now
            )

        WantGoHome ->
            ( { model
                | view = HomeView
                , answerRevealed = False
                , newQuestion = { question = "", answer = "", contentType = Raw }
              }
            , Task.perform NewTime Time.now
            )

        TypeNewQuestion question ->
            ( { model
                | newQuestion =
                    let
                        newQuestion =
                            model.newQuestion
                    in
                    { newQuestion | question = question }
              }
            , Task.perform NewTime Time.now
            )

        TypeNewAnswer answer ->
            ( { model
                | newQuestion =
                    let
                        newQuestion =
                            model.newQuestion
                    in
                    { newQuestion | answer = answer }
              }
            , Task.perform NewTime Time.now
            )

        ChooseAnswerContentType contentType ->
            let
                newQuestion =
                    model.newQuestion
            in
            ( { model
                | newQuestion =
                    { newQuestion
                        | contentType = contentType
                    }
              }
            , Task.perform NewTime Time.now
            )

        SubmitNewCard ->
            let
                newCard =
                    { question = model.newQuestion.question
                    , correctAnswer =
                        { contentType = model.newQuestion.contentType
                        , value = model.newQuestion.answer
                        }
                    , rank = 0
                    , nextReviewAt =
                        Time.millisToPosix <|
                            oneDay
                                + Time.posixToMillis model.now
                    }

                cards =
                    insertCard model.cards newCard
            in
            ( { model
                | newQuestion = { question = "", answer = "", contentType = Raw }
                , cards = cards
              }
            , saveCards <| List.map encodeCard cards
            )

        RevealAnswer ->
            ( { model | answerRevealed = True }, Task.perform NewTime Time.now )

        ValidateAnswer isValid ->
            case List.head model.cards of
                Just card ->
                    let
                        cards =
                            insertCard
                                (case List.tail model.cards of
                                    Just something ->
                                        something

                                    Nothing ->
                                        []
                                )
                                (processCard model.now card isValid)
                    in
                    ( { model
                        | cards = cards
                        , answerRevealed = False
                      }
                    , saveCards <| List.map encodeCard cards
                    )

                Nothing ->
                    ( model, Task.perform NewTime Time.now )

        RemoveCard index ->
            let
                newCards =
                    List.take index model.cards ++ List.drop (index + 1) model.cards
            in
            ( { model | cards = newCards }, saveCards <| List.map encodeCard newCards )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


header : Model -> E.Element Msg
header model =
    E.el
        [ E.centerX
        , Bg.color (E.rgba255 230 230 230 0.8)
        , Bg.gradient
            { angle = 0
            , steps =
                [ E.rgba255 180 205 223 1
                , E.rgba255 196 208 232 0.8
                ]
            }
        , E.width E.fill
        , E.height <| E.px 70
        , Bd.shadow
            { offset = ( 0.0, 1.0 )
            , size = 2.0
            , blur = 2.0
            , color = E.rgba255 200 200 200 0.5
            }
        , F.size 35
        , F.color (E.rgb255 62 65 60)
        ]
    <|
        E.el
            [ E.centerX
            , E.centerY
            ]
        <|
            E.text <|
                formatDate model.now


view : Model -> Html Msg
view model =
    let
        mainView =
            case model.view of
                HomeView ->
                    homeView model

                AnswerQuestionView ->
                    answerView model

                AddQuestionView ->
                    addQuestionView model

                ManageQuestionsView ->
                    manageQuestionView model
    in
    E.layout
        [ Bg.color (E.rgb255 203 200 202)
        , E.height E.fill
        , F.color (E.rgb255 60 90 70)
        ]
    <|
        E.column
            [ E.spacing 20
            , E.centerX
            , Bg.color (E.rgb255 245 245 240)
            , E.width (E.fill |> E.maximum 800)
            , E.height E.fill
            ]
            [ header model
            , E.el [ E.padding 5, E.centerX, E.width E.fill ] mainView
            ]


shadow : E.Attribute Msg
shadow =
    Bd.shadow
        { offset = ( 0.5, 0.5 )
        , size = 1.0
        , blur = 1.5
        , color = E.rgba255 153 160 152 0.3
        }


button : Msg -> String -> E.Element Msg
button msg label =
    I.button
        [ Bg.color (E.rgb255 221 224 228)
        , E.height (E.px 35)
        , E.width (E.px 200)
        , Bd.solid
        , Bd.widthEach
            { bottom = 2
            , left = 1
            , right = 2
            , top = 1
            }
        , Bd.color (E.rgb255 150 150 150)
        , F.color (E.rgb255 93 95 102)
        , shadow
        , E.padding 0
        ]
        { label = E.el [ E.centerX, E.centerY ] <| E.text label
        , onPress = Just msg
        }


homeView : Model -> E.Element Msg
homeView _ =
    E.column
        [ E.paddingXY 10 5
        , E.spacing 6
        , E.centerX
        ]
        [ E.text "What do you want ?"
        , button (UserInteracts ChooseAddQuestion) "Add Questions"
        , button (UserInteracts ChooseAnswerQuestion) "Answer Questions"
        , button (UserInteracts ChooseManageQuestions) "Manage Questions"
        ]


manageQuestionView : Model -> E.Element Msg
manageQuestionView model =
    let
        headerBgColor =
            Bg.color (E.rgba255 128 157 193 0.5)

        headerStyle =
            [ E.padding 10
            , headerBgColor

            -- , F.color (E.rgb255 240 255 250)
            , E.centerY
            ]

        cellStyle =
            [ E.padding 10 ]
    in
    E.column [ E.width E.fill, E.spacing 10 ]
        [ E.table
            [ Bd.solid
            , Bd.width 2
            , E.centerX
            , Bg.color (E.rgb255 250 255 250)
            ]
            { data =
                List.indexedMap
                    (\i card ->
                        { index = i
                        , question = card.question
                        , rank = card.rank
                        , nextReviewAt = card.nextReviewAt
                        }
                    )
                    model.cards
            , columns =
                [ { header = E.el headerStyle <| E.text "question"
                  , width = E.fill
                  , view = \card -> E.el cellStyle <| E.text <| String.left 40 card.question ++ "..."
                  }
                , { header = E.el headerStyle <| E.text "rank"
                  , width = E.px 50
                  , view = \card -> E.el cellStyle <| E.text <| String.fromInt card.rank
                  }
                , { header = E.el headerStyle <| E.text "next review"
                  , width = E.px 200
                  , view = \card -> E.el cellStyle <| E.text <| formatDate card.nextReviewAt
                  }
                , { header =
                        E.el
                            [ headerBgColor
                            , F.color (E.rgb255 240 255 250)
                            , E.centerY
                            , E.height E.fill
                            ]
                        <|
                            E.text " "
                  , width = E.px 30
                  , view =
                        \card ->
                            E.el [ E.centerY, E.spacing 5 ] <|
                                I.button
                                    [ E.centerY
                                    , Bg.color (E.rgb255 230 100 100)
                                    , F.color (E.rgb255 230 200 200)
                                    , E.paddingEach
                                        { top = 2
                                        , bottom = 4
                                        , left = 5
                                        , right = 5
                                        }
                                    , E.centerX
                                    ]
                                    { label = E.el [ E.centerX, E.centerY ] <| E.text "✖"
                                    , onPress = Just <| UserInteracts <| RemoveCard card.index
                                    }
                  }
                ]
            }
        , E.row [ E.centerX ] [ button (UserInteracts WantGoHome) "End" ]
        ]


cardElement : E.Element Msg -> E.Element Msg
cardElement content =
    E.el
        [ E.centerX
        , E.padding 10
        , Bd.solid
        , shadow
        , Bd.width 2
        , Bg.color (E.rgb 1.0 1.0 1.0)
        ]
    <|
        content


answerView : Model -> E.Element Msg
answerView model =
    case selectCard model.now model.cards of
        Nothing ->
            E.column
                [ E.spacing 5, E.padding 10, E.centerX ]
                [ E.el [] <| E.text "Nothing to review. Take a rest."
                , E.el [ E.centerX ] <| button (UserInteracts WantGoHome) "End"
                ]

        Just card ->
            E.column [ E.width E.fill, E.spacing 20 ]
                [ cardElement <|
                    E.text card.question
                , if model.answerRevealed then
                    cardElement <|
                        case card.correctAnswer.contentType of
                            Raw ->
                                E.text card.correctAnswer.value

                            MathFormula ->
                                E.html <|
                                    node "math-formula"
                                        [ attribute "src" card.correctAnswer.value
                                        ]
                                        []

                  else
                    I.button
                        [ Bg.color (E.rgb255 75 112 200)
                        , E.height (E.px 35)
                        , E.width (E.px 200)
                        , Bd.solid
                        , Bd.widthEach
                            { bottom = 2
                            , left = 1
                            , right = 2
                            , top = 1
                            }
                        , Bd.color (E.rgb255 75 112 213)
                        , F.color (E.rgb255 250 250 255)
                        , shadow
                        , E.padding 0
                        , E.centerX
                        ]
                        { label = E.el [ E.centerX, E.centerY ] <| E.text "Reveal answer"
                        , onPress = Just <| UserInteracts RevealAnswer
                        }
                , if model.answerRevealed then
                    E.row [ E.centerX, E.spacing 5 ]
                        [ I.button
                            [ Bg.color (E.rgb255 75 200 112)
                            , E.height (E.px 35)
                            , E.width (E.px 200)
                            , Bd.solid
                            , Bd.widthEach
                                { bottom = 2
                                , left = 1
                                , right = 2
                                , top = 1
                                }
                            , Bd.color (E.rgb255 75 213 112)
                            , F.color (E.rgb255 250 250 255)
                            , shadow
                            , E.padding 0
                            , E.centerX
                            ]
                            { label = E.el [ E.centerX, E.centerY ] <| E.text "You knew it"
                            , onPress = Just <| UserInteracts <| ValidateAnswer True
                            }
                        , I.button
                            [ Bg.color (E.rgb255 200 50 50)
                            , E.height (E.px 35)
                            , E.width (E.px 200)
                            , Bd.solid
                            , Bd.widthEach
                                { bottom = 2
                                , left = 1
                                , right = 2
                                , top = 1
                                }
                            , Bd.color (E.rgb255 213 75 112)
                            , F.color (E.rgb255 250 250 255)
                            , shadow
                            , E.padding 0
                            , E.centerX
                            ]
                            { label = E.el [ E.centerX, E.centerY ] <| E.text "You didn't know"
                            , onPress = Just <| UserInteracts <| ValidateAnswer False
                            }
                        ]

                  else
                    E.none
                , E.row [ E.centerX ] [ button (UserInteracts WantGoHome) "End" ]
                ]


addQuestionView : Model -> E.Element Msg
addQuestionView model =
    E.column
        [ E.centerX
        , E.spacing 5
        ]
        [ I.multiline
            [ E.width (E.px 300) ]
            { onChange = \s -> UserInteracts (TypeNewQuestion s)
            , text = model.newQuestion.question
            , placeholder =
                Just <|
                    I.placeholder [] <|
                        E.text "enter your question here"
            , label = I.labelLeft [ E.width <| E.px 90 ] <| E.text "Question"
            , spellcheck = True
            }
        , I.multiline
            [ E.width (E.px 300) ]
            { onChange = \s -> UserInteracts (TypeNewAnswer s)
            , text = model.newQuestion.answer
            , placeholder = Just <| I.placeholder [] <| E.text "enter the answer here"
            , label = I.labelLeft [ E.width <| E.px 90 ] <| E.text "Answer"
            , spellcheck = True
            }
        , I.radioRow
            [ E.padding 10
            , E.spacing 20
            ]
            { onChange = UserInteracts << ChooseAnswerContentType
            , selected = Just model.newQuestion.contentType
            , label = I.labelLeft [] (E.text "answer type :")
            , options =
                [ I.option Raw (E.text "raw")
                , I.option MathFormula (E.text "latex")
                ]
            }
        , case model.newQuestion.contentType of
            MathFormula ->
                E.html <|
                    node "math-formula"
                        [ attribute "src" model.newQuestion.answer
                        ]
                        []

            _ ->
                E.none
        , E.row []
            [ button (UserInteracts SubmitNewCard) "Submit"
            , button (UserInteracts WantGoHome) "End"
            ]
        ]
