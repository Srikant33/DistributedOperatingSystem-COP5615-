#r "nuget: Akka" 
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit"

open System
open Akka.Actor
open Akka.FSharp
open System.Collections.Generic

let random = System.Random()
let actorSystem = System.create "processor" (Configuration.load())
let runTime = System.Diagnostics.Stopwatch.StartNew()

//Receiving numNodes and numRequests 
let mutable nodesToCheck = int fsi.CommandLineArgs.[1]
let mutable requestsToCheck = int fsi.CommandLineArgs.[2]
let mutable m = 0
let mutable n = nodesToCheck
while n > 0 do
    m <- m + 1
    n <- (n >>> 1)
printfn "Nodes To Be Checked: %A " nodesToCheck
printfn "Requests To be Checked: %A " requestsToCheck
printfn "Finger Table contains %A tupeles." m // Number of tuples needed in finger table to cover the whole cord

let largest = 1 <<< m

type Infostring =
    {
        Stringval: string
        Key: int
    }

type Node = 
    {
        NKey: int
        Reference: IActorRef
    }
    
type MessageQuery =
    {
        From: IActorRef
        Key: int
        Hops: int
    }

type Joiner =
    {
        Predecsssor: Node
        Successsor: Node
    }

type ChangingPredecessor =
    {
        NewPredecessor: Node
    }

type Set =
    {
        Self: int
        NextNode: Node
        PreviousNode: Node
        FingerNodes: Node List
    }

type FingerTableCorrection =
    {
        Fkey: int
        Findex: int
        Ffrom: IActorRef
        Return: bool
        GoalIndex: int
        GoalNode: IActorRef
    }


//Chord Actor
let chordActor (mailbox: Actor<_>) =
    let mutable Key = 0
    let mutable next = {NKey = 0; Reference = mailbox.Self}
    let mutable previous = {NKey = 0; Reference = mailbox.Self}
    let mutable fingerTable = List[]
    let mutable fingerArray = [||]
    let index = 0
    let recordArray = 
        [| 
            for i in 0 .. 16 do
                yield {Stringval = ""; Key = -1}
        |]

//to find the node which is the nearest before the node we are looking for 
    let nearestPreviousNode id =
        let mutable toPrevious = -1
        for i = m-1 downto 0 do
            let jumpTowards = if id < Key then 
                                id + largest else id
            let a = if fingerTable.[i].NKey < Key then 
                        fingerTable.[i].NKey + largest 
                    else 
                        fingerTable.[i].NKey
            if toPrevious = -1 && a > Key && a < jumpTowards then
                toPrevious <- i
        if toPrevious = -1 then
            mailbox.Self
        else 
            fingerTable.[toPrevious].Reference
       
    let rec loop() = actor {
        let! message = mailbox.Receive()
        let sender = mailbox.Sender()
        match box message with
        | :? int as id -> 
            let target = nearestPreviousNode id
            if (target) = mailbox.Self then
                ()
                // printfn "Node %A reached" next.NKey
            else
                target <! id

        | :? ChangingPredecessor as u ->
            previous <- u.NewPredecessor
        
        | :? Joiner as itj ->
            previous <- itj.Predecsssor
            next <- itj.Successsor
            for i = 0 to m - 1 do
                next.Reference <! {Fkey = Key + (1 <<< i);Findex = i; Ffrom = mailbox.Self; Return = false;GoalIndex = -1; GoalNode = mailbox.Self}
        
        | :? FingerTableCorrection as fi ->
            if fi.Return = false then
                let target = nearestPreviousNode fi.Fkey
                if target = mailbox.Self then
                    ()
                    //printfn "Node %A reached" next.NKey
                    fi.Ffrom <! {Ffrom = fi.Ffrom; Fkey = fi.Fkey; Findex = fi.Findex; Return = true; GoalIndex = next.NKey; GoalNode = next.Reference}
                else
                    target <! fi
            else
                Array.set fingerArray fi.Findex {NKey = fi.GoalIndex; Reference = fi.GoalNode}
        
        | :? Set as info ->
            Key <- info.Self
            next <- info.NextNode
            previous <- info.PreviousNode
            fingerTable <- info.FingerNodes
                
        | :? Node as info ->
            let target = nearestPreviousNode info.NKey
            if target = mailbox.Self then
                ()
                let itj = {Predecsssor = {NKey = Key; Reference = mailbox.Self}; Successsor = {NKey = next.NKey; Reference = next.Reference}}
                info.Reference <! itj
                next.Reference <! {NewPredecessor = info}
                next <- info
                for i = 0 to m - 1 do
                    next.Reference <! {Fkey = Key + (1 <<< i);Findex = i; Ffrom = mailbox.Self; Return = false;GoalIndex = -1; GoalNode = mailbox.Self}
            else
                target <! info
        
        | :? MessageQuery as q ->
            let target = nearestPreviousNode q.Key
            if (target) = mailbox.Self then
                ()
                //printfn "Node %A reached" next.NKey
                q.From <! q
            else
                target <! {Key = q.Key; Hops = q.Hops + 1; From = q.From}
       
        | _ -> ()
            
        return! loop()
    }
    loop()

//Random value generated to set as nodes
let rands = [
    for i in 1 .. nodesToCheck do
        yield (random.Next() % largest)
]
let numList = List.sort rands

let actorList = [
    for i in 0 .. (nodesToCheck - 1) do
        let name = "Actor" + i.ToString()
        let temp = spawn actorSystem name chordActor
        yield temp
]

for i in 0 .. (nodesToCheck - 1) do
    let FList = List[
        for j in 0 .. (m-1) do
            let toPrevious = 1 <<< j
            let mutable mark = -1;
            for k in 1 .. nodesToCheck do
                let a = if i + k >= nodesToCheck then 
                            numList.[(i+k) % nodesToCheck] + largest 
                        else 
                            numList.[(i+k) % nodesToCheck]
                if a >= toPrevious + numList.[i] && mark = -1 then
                    mark <- (i+k) % nodesToCheck
            let temp = {NKey = numList.[mark]; Reference = actorList.[mark]}
            yield temp
    ]
     
    let myInfo = {Self = numList.[i];
                     NextNode = {NKey = numList.[(i+1) % nodesToCheck];
                     Reference = actorList.[(i+1) % nodesToCheck]}; 
                     PreviousNode = {NKey = numList.[(i+nodesToCheck-1) % nodesToCheck]; 
                     Reference = actorList.[(i+nodesToCheck-1) % nodesToCheck]}; 
                     FingerNodes = FList }
    actorList.[i] <! myInfo
    actorList.[i] <! myInfo

let inputStringList = [
    for i in 1 .. nodesToCheck do
        let requestList = [
            for j in 1 .. requestsToCheck do
                let l = random.Next() % 100 + 1
                let mutable s = ""
                for k in 0 .. l do
                    s <- s + char(random.Next() % 95 + 32).ToString()
                yield s
        ]
        yield requestList
]

// Using SHA1 to get string
let sha1 (s:string) =
    let stringbyts = System.Text.Encoding.ASCII.GetBytes(s)
    let encoded = System.Security.Cryptography.SHA1.Create().ComputeHash(stringbyts)
    let mutable hashed = 0
    for i in 0 .. (encoded.Length - 1) do
        hashed <- hashed + (int encoded.[i])
    if hashed < 0 then
        hashed <- 0 - hashed
    hashed


//Dispatcher actor to call worker actors
printfn " "
let dispatcher = 
    spawn actorSystem "dispatcher" 
        (fun mailbox ->
            let mutable counter = 0
            let mutable total = 0
            let rec loop() = actor {
                let! message = mailbox.Receive()
                let sender = mailbox.Sender()
                match box message with
                | :? string as s ->
                    if s = "Chord Connect" then
                        for i in 0 .. (nodesToCheck - 1) do
                            for j in 0 .. (requestsToCheck - 1) do
                                
                                actorList.[i] <! {Key = (sha1 inputStringList.[i].[j]) % largest; Hops = 0; From = mailbox.Self}
                | :? MessageQuery as q ->
                    total <- total + q.Hops
                    counter <- counter + 1
                    if counter = nodesToCheck * requestsToCheck then
                        printfn "Hop average: %f" ((float total) / (float counter))
                        printfn "Time taken time is: %f ms" runTime.Elapsed.TotalMilliseconds
                | _ -> ()

                return! loop()
            }
            loop()
        )

dispatcher <! "Chord Connect"

Console.ReadLine() |> ignore