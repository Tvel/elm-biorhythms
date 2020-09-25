module People exposing (..)

import Dict
import Time exposing (Month)

type alias Name = String
type alias Person =
    { day : Int, month : Month, year : Int, name : Name }

get: List Person -> Name -> Maybe Person
get people name=
    List.head (List.filter (\p -> p.name == name) people)

update: List Person -> Person -> List Person
update people person =
    Dict.fromList (List.map (\v -> (.name v, v)) people)
    |> Dict.insert person.name person
    |> Dict.values
    |> List.sortBy .name

remove: List Person -> Name -> List Person
remove people name=
    List.filter (\p -> p.name /= name) people