#r "nuget: Akka" 
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.Remote" 
#r "nuget: Akka.TestKit" 

open System
open System.Threading
open Akka.Actor
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
type Bundle_Actor_P = Bundle8 of  string  * string * string* string* string * string* string* string * string

// number of user
let args : string array = fsi.CommandLineArgs |> Array.tail
let N= args.[0] |> int
let M = N
let mutable i = 0
let mutable ii = 0
let obj = new Object()
let addIIByOne() =
    Monitor.Enter obj
    ii<- ii+1
    Monitor.Exit obj
    
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
                maximum-payload-bytes = 30000000 bytes
                helios.tcp {
                    port = 8123
                    hostname = localhost
                    message-frame-size =  30000000b
      send-buffer-size =  30000000b
      receive-buffer-size =  30000000b
      maximum-frame-size = 30000000b
                }
            }
        }")
let system = ActorSystem.Create("RemoteFSharp", configuration)

let echoServer = system.ActorSelection(
                            "akka.tcp://RemoteFSharp@localhost:8777/user/EchoServer")

let rand = System.Random(1)
let akka_user_register (mailbox: Actor<_>) = 
    let rec loop () = actor {
        let! message = mailbox.Receive()
        let idx = message
        let mutable req = "reg"           
        let mutable POST = " "
        let mutable user_name = "user"+(string idx)
        let mutable pwd = "pwd" + (string idx)
        let mutable to_user = " "
        let mutable hash_mentioned = " "
        let mutable at = " "
        let mutable message_string = " "
        let mutable register = " "
        let cmd = req+","+POST+","+user_name+","+pwd+","+to_user+","+message_string+","+hash_mentioned+","+at+","+register
        let task = echoServer <? cmd
        let response = Async.RunSynchronously (task, 1000)
        //printf "[command]%s" cmd
        //printfn "[Reply]%s" (string(response))
        printf "%s" ""
        addIIByOne()
        return! loop()
    }
    loop ()
let akka_client_simulator (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        let idx = message
        match box message with
        | :? string   ->
            let mutable rand_num = Random( ).Next() % 7
            let mutable req = "reg"           
            let mutable POST = "POST"
            let mutable user_name = "user"+(string idx)
            let mutable pwd = "pwd" + (string idx)
            let mutable to_user = "user"+rand.Next(N) .ToString()
            let mutable hash_mentioned = "#topic"+rand.Next(N) .ToString()
            let mutable at = "@user"+rand.Next(N) .ToString()
            let mutable message_string = "tweet"+rand.Next(N) .ToString()+"... " + hash_mentioned + "..." + at + " " 
            let mutable register = "register"
            if rand_num=0 then  req <-"reg"
            if rand_num=1 then  req <-"send"
            if rand_num=2 then  req <-"subscribe"
            if rand_num=3 then  req <-"retweet"
            if rand_num=4 then  req <-"querying"
            if rand_num=5 then  req <-"#"
            if rand_num=6 then  req <-"@" 
            // msg can be anything like "start"
            let cmd = req+","+POST+","+user_name+","+pwd+","+to_user+","+message_string+","+hash_mentioned+","+at+","+register
            let task = echoServer <? cmd
            let response = Async.RunSynchronously (task, 3000)
            //printfn "[command]%s" cmd
            //printfn "[Reply]%s" (string(response))
            printf "%s" ""
            addIIByOne()
        return! loop()     
    }
    loop ()

let client_user_register = spawn system "client_user_register" akka_user_register    
let client_simulator = spawn system "client_simulator" akka_client_simulator

//printfn "Creating acounts" 

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
i<-0
ii<-0
while i<N do
    client_user_register <! string i |>ignore
    i<-i+1
while ii<N-1 do
    Thread.Sleep(50)
stopWatch.Stop()
let time_register = stopWatch.Elapsed.TotalMilliseconds
//    Thread.Sleep(5000)




printfn "send tweet" 
stopWatch = System.Diagnostics.Stopwatch.StartNew()
for i in 0..N-1 do
    for j in 0..25 do
        let cmd = "send, ,user"+(string i)+",pwd"+(string i)+", ,tweet+user"+(string i)+"_"+(string j)+"th @user"+(string (rand.Next(N)))+" #topic"+(string (rand.Next(N)))+" , , , "
//            let cmd = "send, ,user"+(string i)+",pwd"+(string i)+", ,@user"+(string (rand.Next(N)))+" #topic"+(string (rand.Next(N)))+" , , , "
//            let cmd = "send, ,user"+(string i)+",pwd"+(string i)+", ,t, , , "
        let task = echoServer <? cmd
        let response = Async.RunSynchronously (task, 3000)
        //printfn "[command]%s" cmd
        //printfn "[Reply]%s" (string(response))
        printf "%s" ""
stopWatch.Stop()
let time_send = stopWatch.Elapsed.TotalMilliseconds





let mutable step = 1
stopWatch = System.Diagnostics.Stopwatch.StartNew()
printfn "Zipf Subscribe"  
for i in 0..N-1 do
    for j in 0..step..N-1 do
        if not (j=i) then
            let cmd = "subscribe, ,user"+(string j)+",pwd"+(string j)+",user"+(string i)+", , , , "
            let task = echoServer <? cmd
            let response = Async.RunSynchronously (task, 3000)
            //printfn "[command]%s" cmd
            //printfn "[Reply]%s" (string(response))
            printf "%s" ""
        step <- step+1
stopWatch.Stop()
let time_zipf_subscribe = stopWatch.Elapsed.TotalMilliseconds
    


stopWatch = System.Diagnostics.Stopwatch.StartNew()
for i in 0..N-1 do
    let cmd = "querying, ,user"+(string i)+",pwd"+(string i)+", , , , , "
    let task = echoServer <? cmd
    let response = Async.RunSynchronously (task, 5000)
    //printfn "[command]%s" cmd
    //printfn "[Reply]%s" (string(response))
    printf "%s" ""
stopWatch.Stop()
let time_query = stopWatch.Elapsed.TotalMilliseconds



stopWatch = System.Diagnostics.Stopwatch.StartNew()
for i in 0..N-1 do
    let cmd = "#, , , , , ,#topic"+(string (rand.Next(N)))+", ,"
    let task = echoServer <? cmd
    let response = Async.RunSynchronously (task, 3000)
    //printfn "[command]%s" cmd
    //printfn "[Reply]%s" (string(response))
    printf "%s" ""
stopWatch.Stop()
let time_hashtag = stopWatch.Elapsed.TotalMilliseconds




stopWatch = System.Diagnostics.Stopwatch.StartNew()
for i in 0..N-1 do
    let cmd = "@, , , , , , ,@user"+(string (rand.Next(N)))+","
    let task = echoServer <? cmd
    let response = Async.RunSynchronously (task, 3000)
    //printfn "[command]%s" cmd
    //printfn "[Reply]%s" (string(response))
    printf "%s" ""
stopWatch.Stop()
let time_mention = stopWatch.Elapsed.TotalMilliseconds




printfn "%d Random Ops and send tweet" M 

stopWatch = System.Diagnostics.Stopwatch.StartNew()
i<-0
ii<-0
while i<M do
    client_simulator<! string (rand.Next(N)) |>ignore
    i <- i+1
while ii<M-1 do
    Thread.Sleep(50)
stopWatch.Stop()
let time_random = stopWatch.Elapsed.TotalMilliseconds


printfn "Time for largest operation %f" time_send
printfn "To %d random operations is %f" M time_random
printfn " "

printfn "Total Result: %f %f %f %f %f %f %f" time_register time_send time_zipf_subscribe time_query time_hashtag time_mention time_random


system.Terminate() |> ignore
0 // return an integer exit code

