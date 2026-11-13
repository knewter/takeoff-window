module Rng exposing (nextFloat, seedFrom)

import Bitwise


{-| mulberry32, matching the JavaScript implementation bit-for-bit.
The stored state is the generator's `a` register.
-}
nextFloat : Int -> ( Float, Int )
nextFloat a0 =
    let
        a =
            signed32 (signed32 a0 + 0x6D2B79F5)

        t1 =
            imul (Bitwise.xor a (Bitwise.shiftRightZfBy 15 a)) (Bitwise.or 1 a)

        t2 =
            Bitwise.xor (t1 + imul (Bitwise.xor t1 (Bitwise.shiftRightZfBy 7 t1)) (Bitwise.or 61 t1)) t1

        mixed =
            Bitwise.shiftRightZfBy 0 (Bitwise.xor t2 (Bitwise.shiftRightZfBy 14 t2))
    in
    ( toFloat mixed / 4294967296, a )


seedFrom : String -> Int
seedFrom raw =
    let
        s =
            String.trim raw
    in
    if s == "" then
        1

    else if String.all Char.isDigit s && String.length s <= 15 then
        s
            |> String.toInt
            |> Maybe.withDefault 1
            |> unsigned32

    else
        fnv1a s


fnv1a : String -> Int
fnv1a s =
    String.toList s
        |> List.foldl (\c h -> imul (Bitwise.xor h (Char.toCode c)) 16777619) (signed32 2166136261)
        |> unsigned32


signed32 : Int -> Int
signed32 n =
    Bitwise.or n 0


unsigned32 : Int -> Int
unsigned32 n =
    Bitwise.shiftRightZfBy 0 n


{-| Low 32 bits of a product, as a signed int. Avoids JS float rounding. -}
imul : Int -> Int -> Int
imul a b =
    let
        a0 =
            Bitwise.and a 65535

        a1 =
            Bitwise.and (Bitwise.shiftRightZfBy 16 a) 65535

        b0 =
            Bitwise.and b 65535

        b1 =
            Bitwise.and (Bitwise.shiftRightZfBy 16 b) 65535

        lo =
            a0 * b0

        mid =
            a1 * b0 + a0 * b1 + Bitwise.shiftRightZfBy 16 lo
    in
    Bitwise.or (Bitwise.and lo 65535) (Bitwise.shiftLeftBy 16 (Bitwise.and mid 65535))
