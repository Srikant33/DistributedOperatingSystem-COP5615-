
#r "nuget: Akka" 
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.Remote" 
#r "nuget: Akka.TestKit" 

open Akka.Actor
open Akka.FSharp
open System.Threading
open System
open Akka.Configuration
type Bundle_Actor_P = Bundle8 of  string  * string * string* string* string * string* string* string * string

//usrs
let args : string array = fsi.CommandLineArgs |> Array.tail
let mutable N= args.[0] |> int

let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
            log-config-on-start : on
            stdout-loglevel : DEBUG
            loglevel : ERROR
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
                debug : {
                    receive : on
                    autoreceive : on
                    lifecycle : on
                    event-stream : on
                    unhandled : on
                }
            }
            remote {
                helios.tcp {
                    port = "+string (System.Random( ).Next(10000,20000))+" 
                    hostname = localhost
                }
            }
        }")
let system = ActorSystem.Create("RemoteFSharp", configuration)

let echoServer = system.ActorSelection("akka.tcp://RemoteFSharp@localhost:8777/user/EchoServer")

let mutable prev_query = ""
let mutable auto = false
let akka_user_connect (mailbox: Actor<_>) = 
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        match message with
        | user_name,pwd ->
            while not auto do
                Thread.Sleep(500)
            let task = echoServer <?  "querying, ," + user_name + "," + pwd + ", , , , , "
            let response = Async.RunSynchronously (task, 1000) |> string
            if not (response = prev_query) then
                prev_query <- response
                printfn "[AutoQuery]%s" response
                printfn "%s" ""
            Thread.Sleep(1000)
            mailbox.Self <? (user_name, pwd) |> ignore
            return! loop() 
    }
    loop ()
    
let client_user_connect = spawn system "client_user_connect" akka_user_connect

let akka_user (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let cmd = Console.ReadLine()
        let result = cmd.Split ','
        let req = result.[0]
        if req="connect" then
            let user_name=result.[2]
            let pwd=result.[3]
            auto <- true
            client_user_connect <? (user_name, pwd) |> ignore
            return! loop() 
        else if req="disconnect" then
            auto <- false
            return! loop() 
        let task = echoServer <? cmd
        let response = Async.RunSynchronously (task, 1000)
        printfn "[Reply]%s" (string(response))
        printfn "%s" ""
        return! loop()     
    }
    loop ()

let client_user = spawn system "client_user" akka_user


printfn " Please select an operation : reg  " 
client_user <? "go" |>ignore
Thread.Sleep(1000000)
system.Terminate() |> ignore

0 