#time "on"
#r @"bin/Debug/netcoreapp3.1/Akka.dll"
#r @"bin/Debug/netcoreapp3.1/Akka.FSharp.dll"

open System
open System.Collections.Generic
open Akka.Actor
open Akka.Configuration
open Akka.FSharp

let system = ActorSystem.Create("FSharp")

let args = fsi.CommandLineArgs
let mutable numNodes = args.[1] |> int
let topology = args.[2].ToLower() 
let algo = args.[3].ToLower()
let mutable convergenceCounter = 0
let time = Diagnostics.Stopwatch()
let gossipCutOff = 2


type ActorMsg = 
    | TelGossip of string * int * list<IActorRef>
    | Initialise of list<IActorRef>

type EchoServer(actr) =
   inherit Actor()
   let mutable gossipCounter = 0
   let mutable neighbor = []
   override x.OnReceive(message) =
      match message :?> ActorMsg with
        | TelGossip (rumour,node,listOfActors)->
            gossipCounter <- gossipCounter + 1
            let rd = System.Random()
            let rnd  = rd.Next(1, neighbor.Length)

            if (gossipCounter = gossipCutOff) then
               convergenceCounter <- convergenceCounter + 1
               
            if (numNodes/2 <= convergenceCounter) then 
                time.Stop()
                System.Console.WriteLine("Time elapsed: {0}", time.Elapsed)
                Environment.Exit(0)
                
            neighbor.Item(rnd) <! TelGossip (rumour,rnd,listOfActors)


        | Initialise (neighborList) -> 
            neighbor <- neighborList    
                    
let makePerfectSquare (a: int) : int =
    let mutable x = float (a)
    let squareroot = sqrt (x)
    if squareroot % 1.0 <> 0.0 then
       while sqrt (x) % 1.0 <> 0.0 do
        x <- x + float(1)   
    int(x)

if topology = "2d" then 
    numNodes <-  makePerfectSquare numNodes 

let children = 
    [1 .. numNodes+1]
    |> List.map(fun id ->   let properties = [| string(id) :> obj |]
                            system.ActorOf(Props(typedefof<EchoServer>, properties)))

let topologyNeighbour nodeVal  : list<IActorRef> = 
    
    let mutable neighbourList: list<IActorRef> = []
    match topology with 
     | "full" -> 
                 for j in [1 .. numNodes] do
                    if (j <> nodeVal) then
                        neighbourList <- children.Item(j) :: neighbourList
                        

     | "line" -> 
                 if(nodeVal = 1)  then
                     neighbourList <- children.Item(nodeVal+1) :: neighbourList
                     
                 else if(nodeVal = numNodes)  then
                     neighbourList <- children.Item(nodeVal-1) :: neighbourList
                     
                 else 
                     neighbourList <- children.Item(nodeVal-1) :: neighbourList
                     neighbourList <- children.Item(nodeVal+1) :: neighbourList
     | "2d" ->
                
                let edgeSize  = sqrt (float (numNodes)) |> int
                let nodeArray = Array.zeroCreate (numNodes)
                for i in [ 0 .. (edgeSize * edgeSize - 1) ] do
                    nodeArray.[i] <- i+1
                let temp = nodeVal-1
                let i = temp / edgeSize
                let j = temp % edgeSize

                if j + 1 < edgeSize then 
                            neighbourList <- children.Item(nodeArray.[i * edgeSize + j + 1]) :: neighbourList 
                if j - 1 >= 0 then
                            neighbourList <- children.Item(nodeArray.[i * edgeSize + j - 1]) :: neighbourList
                if i - 1 >= 0 then
                            neighbourList <- children.Item(nodeArray.[(i - 1) * edgeSize + j]) :: neighbourList
                if i + 1 < edgeSize then 
                            neighbourList <- children.Item(nodeArray.[(i + 1) * edgeSize + j]) :: neighbourList
                              
     | "imp2d" ->
                
                let edgeSize  = sqrt (float (numNodes)) |> int
                let nodeArray = Array.zeroCreate (numNodes)
                for i in [ 0 .. (edgeSize * edgeSize - 1) ] do
                    nodeArray.[i] <- i+1
                let temp = nodeVal-1
                let i = temp / edgeSize
                let j = temp % edgeSize

                if j + 1 < edgeSize then 
                            neighbourList <- children.Item(nodeArray.[i * edgeSize + j + 1]) :: neighbourList 
                if j - 1 >= 0 then
                            neighbourList <- children.Item(nodeArray.[i * edgeSize + j - 1]) :: neighbourList
                if i - 1 >= 0 then 
                            neighbourList <- children.Item(nodeArray.[(i - 1) * edgeSize + j]) :: neighbourList
                if i + 1 < edgeSize then 
                            neighbourList <- children.Item(nodeArray.[(i + 1) * edgeSize + j]) :: neighbourList                 
                let random = System.Random()
                let a  = random.Next(0, numNodes-1)
                neighbourList <- children.Item(nodeArray.[a]) :: neighbourList
     | _ -> failwith "unknown input"

    neighbourList  


if topology = "2d" then 
    numNodes <-  makePerfectSquare numNodes          

for i in [1 .. numNodes] do
    children.Item(i) <! Initialise(topologyNeighbour i)       


time.Start() 
let random = System.Random()
let a  = random.Next(1, numNodes)
children.Item(a) <! TelGossip ("Rumour",a,children)
System.Console.ReadLine() |> ignore



    
        
