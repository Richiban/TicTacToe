

type PlayerToken = X | O with static member Swap = function X -> O | O -> X

type BoardCell = Empty | Taken of PlayerToken

type Location = A1 | B1 | C1 | A2 | B2 | C2 | A3 | B3 | C3 with
    static member All = [ A1; B1; C1; A2; B2; C2; A3; B3; C3 ]

let pos1 (x, _, _) = x
let pos2 (_, y, _) = y
let pos3 (_, _, z) = z

let (|Ref|) r = !r

type Board () =
    let cells = (
        (ref Empty, ref Empty, ref Empty),
        (ref Empty, ref Empty, ref Empty),
        (ref Empty, ref Empty, ref Empty)
    )
    
    let tracks =
        seq {
            //  Diagonals
            yield cells |> pos1 |> pos1, cells |> pos2 |> pos2, cells |> pos3 |> pos3
            yield cells |> pos3 |> pos1, cells |> pos2 |> pos2, cells |> pos1 |> pos3
            
            //  Horizontals
            yield cells |> pos1 |> pos1, cells |> pos1 |> pos2, cells |> pos1 |> pos3
            yield cells |> pos2 |> pos1, cells |> pos2 |> pos2, cells |> pos2 |> pos3
            yield cells |> pos3 |> pos1, cells |> pos3 |> pos2, cells |> pos3 |> pos3
            
            //  Verticals
            yield cells |> pos1 |> pos1, cells |> pos2 |> pos1, cells |> pos3 |> pos1
            yield cells |> pos1 |> pos2, cells |> pos2 |> pos2, cells |> pos3 |> pos2
            yield cells |> pos1 |> pos3, cells |> pos2 |> pos3, cells |> pos3 |> pos3
        }
        
    member this.Tracks = tracks
    
    member this.IsFull = this.AvailableLocations |> Seq.isEmpty
        
    member this.AvailableLocations =
        Location.All |> Seq.filter ( fun location -> match this.GetCell location with Ref Empty -> true | _ -> false )
    
    member this.Place token location =
        match this.GetCell location with
        | Ref Empty as cell -> cell := Taken token
        | Ref (Taken occupier) -> failwithf "Cannot place token '%A' at location '%A'. It is already '%A'" token location occupier
    
    member this.GetCell =
        function
        | A1 -> cells |> pos1 |> pos1
        | B1 -> cells |> pos1 |> pos2
        | C1 -> cells |> pos1 |> pos3
        | A2 -> cells |> pos2 |> pos1
        | B2 -> cells |> pos2 |> pos2
        | C2 -> cells |> pos2 |> pos3
        | A3 -> cells |> pos3 |> pos1
        | B3 -> cells |> pos3 |> pos2
        | C3 -> cells |> pos3 |> pos3
        
    override this.ToString() =
        let printCell (Ref cell) = match cell with Empty -> " " | Taken X -> "X" | Taken O -> "O"
        let printLocation = this.GetCell >> printCell
        
        sprintf "
| %s | %s | %s |
| %s | %s | %s |
| %s | %s | %s |" (printLocation A1) (printLocation B1) (printLocation C1) (printLocation A2) (printLocation B2) (printLocation C2) (printLocation A3) (printLocation B3) (printLocation C3) 
        

type BoardState = {
    playerToGo: PlayerToken
    board: Board
} with
    static member Default = { playerToGo = X; board = Board() }

type GameState = Playing | Winner of PlayerToken | Stalemate

let random = System.Random()

let getPlayerMoveLocation (board : Board) =
    let locations = board.AvailableLocations |> Array.ofSeq
    
    let index = random.Next() % locations.Length
    
    locations.[index]
    
let getState (board : Board) =
    let maybeWinner = 
        board.Tracks |> Seq.tryPick(fun track ->
            match track with
            | Ref (Taken c1), Ref (Taken c2), Ref (Taken c3) when c1 = c2 && c2 = c3 -> Some c1
            | _ -> None)
            
    match maybeWinner with
    | None when board.IsFull -> Stalemate
    | None -> Playing
    | Some winner -> Winner winner

let rec GameTurn state =
    state.board |> printfn "%A"
    
    match getState state.board with
    | Winner winner ->
        printfn "We have a winner: %A" winner
    | Playing ->
        let moveLocation = getPlayerMoveLocation state.board
        
        state.board.Place state.playerToGo moveLocation
        
        GameTurn { state with playerToGo = PlayerToken.Swap state.playerToGo }
    | Stalemate -> 
        printfn "Oh dear, it appears nobody has won this game"
    
        
       
let board = Board()

GameTurn BoardState.Default
