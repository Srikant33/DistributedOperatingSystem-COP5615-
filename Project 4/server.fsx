//all in 1 
//sacerver
#r "nuget: Akka" 
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.Remote" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.FSharp
open Akka.Configuration

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
                    port = 8777
                    hostname = localhost               
                    message-frame-size =  30000000b
                    send-buffer-size =  30000000b
                    receive-buffer-size =  30000000b
                    maximum-frame-size = 30000000b
                }
            }
        }")
//let system = System.create "RemoteFSharp" <| Configuration.load ()
//let system = System.create "RemoteFSharp" ("Actor_P", configuration)
let system = ActorSystem.Create("RemoteFSharp", configuration)
//let system = ActorSystem.Create("RemoteFSharp", configuration)


type Tweet(message_id:string, text:string, forward_message:bool) =
    member this.message_id = message_id
    member this.text = text
    member this.forward_message = forward_message
    override this.ToString() =
      let mutable res = ""
      if forward_message then
        res <- sprintf "[retweet][%s]%s" this.message_id this.text
      else
        res <- sprintf "[%s]%s" this.message_id this.text
      res


type User(usr:string, pwd:string) =
    let mutable sub = List.empty: User list
    let mutable msg = List.empty: Tweet list
    member this.usr = usr
    member this.pwd = pwd
    member this.newsub x =
        sub <- List.append sub [x]
    member this.getSub() =
        sub
    member this.addTweet x =
        msg <- List.append msg [x]
    member this.getMsg() =
        msg
    override this.ToString() = 
       this.usr
       

type Twitter() =
    let mutable msg = new Map<string,Tweet>([])
    let mutable users = new Map<string,User>([])
    let mutable hashtags = new Map<string, Tweet list>([])
    let mutable mentions = new Map<string, Tweet list>([])
    member this.AddTweet (tweet:Tweet) =
        msg <- msg.Add(tweet.message_id,tweet)
    member this.AddUsr (user:User) =
        users <- users.Add(user.usr, user)
    member this.AddToHashTag hashtag tweet =
        let key = hashtag
        let mutable map = hashtags
        if map.ContainsKey(key)=false
        then
            let l = List.empty: Tweet list
            map <- map.Add(key, l)
        let value = map.[key]
        map <- map.Add(key, List.append value [tweet])
        hashtags <- map
    member this.AddToMention mention tweet = 
        let key = mention
        let mutable map = mentions
        if map.ContainsKey(key)=false
        then
            let l = List.empty: Tweet list
            map <- map.Add(key, l)
        let value = map.[key]
        map <- map.Add(key, List.append value [tweet])
        mentions <- map
    member this.register user_name pwd =
        let mutable res = ""
        if users.ContainsKey(user_name) then
            res <- "sorry-  user_name already exist"
        else
            let user = new User(user_name, pwd)
            this.AddUsr user
            user.newsub user
            res <- "Register success user_name: " + user_name + "  pwd: " + pwd
        res
    member this.SendTweet user_name pwd text forward_message =
        let mutable res = ""
        if not (this.authentication user_name pwd) then
            res <- "sorry-  authentication fail"
        else
            if users.ContainsKey(user_name)=false then
                res <-  "sorry-  no this user_name"
            else
                let tweet = new Tweet(System.DateTime.Now.ToFileTimeUtc() |> string, text, forward_message)
                let user = users.[user_name]
                user.addTweet tweet
                this.AddTweet tweet
                let idx1 = text.IndexOf("#")
                if not (idx1 = -1) then
                    let idx2 = text.IndexOf(" ",idx1)
                    let hashtag = text.[idx1..idx2-1]
                    this.AddToHashTag hashtag tweet
                let idx1 = text.IndexOf("@")
                if not (idx1 = -1) then
                    let idx2 = text.IndexOf(" ",idx1)
                    let mention = text.[idx1..idx2-1]
                    this.AddToMention mention tweet
                res <-  "--sent-- sent twitter: " + tweet.ToString()
        res
    member this.authentication user_name pwd =
            let mutable res = false
            if not (users.ContainsKey(user_name)) then
//                printfn "%A" "sorry-  no this user_name"
            else
                let user = users.[user_name]
                if user.pwd = pwd then
                    res <- true
            res
    member this.getUser user_name = 
        let mutable res = new User("","")
        if not (users.ContainsKey(user_name)) then
            printfn "%A" "sorry-  no this user_name"
        else
            res <- users.[user_name]
        res
    member this.subscribe user_name1 pwd user_name2 =
        let mutable res = ""
        if not (this.authentication user_name1 pwd) then
            res <- "sorry-  authentication fail"
        else
            let user1 = this.getUser user_name1
            let user2 = this.getUser user_name2
            user1.newsub user2
            res <- "--sent-- " + user_name1 + " subscribe " + user_name2
        res
    member this.forward user_name pwd text =
        let res = "[retweet]" + (this.SendTweet user_name pwd text true)
        res
    member this.queryTweetsSubscribed user_name pwd =
        let mutable res = ""
        if not (this.authentication user_name pwd) then
            res <- "sorry-  authentication fail"
        else
            let user = this.getUser user_name
            let res1 = user.getSub() |> List.map(fun x-> x.getMsg()) |> List.concat |> List.map(fun x->x.ToString()) |> String.concat "\n"
            res <- "--sent-- queryTweetsSubscribed" + "\n" + res1
        res
    member this.queryHashTag hashtag =
        let mutable res = ""
        if not (hashtags.ContainsKey(hashtag)) then
            res <- "sorry-  no this hashtag"
        else
            let res1 = hashtags.[hashtag] |>  List.map(fun x->x.ToString()) |> String.concat "\n"
            res <- "--sent-- queryHashTag" + "\n" + res1
        res
    member this.queryMention mention =
        let mutable res = ""
        if not (mentions.ContainsKey(mention)) then
            res <- "sorry-  no this mention"
        else
            let res1 = mentions.[mention] |>  List.map(fun x->x.ToString()) |> String.concat "\n"
            res <-  "--sent-- queryMention" + "\n" + res1
        res
    override this.ToString() =
        "print the entire Twitter"+ "\n" + msg.ToString() + "\n" + users.ToString() + "\n" + hashtags.ToString() + "\n" + mentions.ToString()
        
    
let twitter = new Twitter()


type Bundle_reg = Bundle1 of  string  * string  * string* string
type Bundle_send = Bundle2 of  string  * string  * string* string* bool
type Bundle_subscribe = Bundle3 of  string  * string  * string* string 
type Bundle_remsg = Bundle4 of  string  * string  * string * string
type Bundle_queryTweetsSubscribed = Bundle5 of  string  * string  * string 
type Bundle_hashtag = Bundle6 of  string  * string   
type Bundle_at = Bundle7 of  string  * string  

let akka_reg (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle1(POST,register,user_name,pwd) ->
            let res = twitter.register user_name pwd
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let akka_send (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        let sender_path = mailbox.Sender().Path.ToStringWithAddress()
        match message  with
        |   Bundle2(POST,user_name,pwd,message_string,false) -> 
            let res = twitter.SendTweet user_name pwd message_string false
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let akka_subscribe (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle3(POST,user_name,pwd,to_user) -> 
            let res = twitter.subscribe user_name pwd to_user
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let akka_retweet (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle4(POST,user_name,pwd,message_string) -> 
            let res = twitter.forward  user_name pwd message_string
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let akka_querying (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle5(POST,user_name,pwd ) -> 
            let res = twitter.queryTweetsSubscribed  user_name pwd
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let akka_queryHashTag (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle6(POST,hash_mentioned) -> 
            let res = twitter.queryHashTag  hash_mentioned
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let akka_at (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle7(POST,at) -> 
            let res = twitter.queryMention  at
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

let mutable req= "reg" 
let mutable POST="POST"
let mutable user_name="user2"
let mutable pwd="123456"
let mutable register="register"
let mutable to_user="user1"
let mutable message_string="Today is a good day!"
let mutable hash_mentioned="#Trump"
let mutable at="@Biden"

type Bundle_Actor_P = Bundle8 of  string  * string * string* string* string * string* string* string * string


let akka_REG = spawn system "Actor_P1" akka_reg
let akka_SEND = spawn system "Actor_P2" akka_send
let akka_SUBSCRIBE = spawn system "Actor_P3" akka_subscribe
let akka_RETWEET = spawn system "Actor_P4" akka_retweet
let akka_QUERYING = spawn system "Actor_P5" akka_querying 
let akka_QUERHASHTAG = spawn system "Actor_P6" akka_queryHashTag
let akka_AT = spawn system "Actor_P7" akka_at

// for the server, received string and dispatch
let akka_incomingMsg (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match box message with
        | :? string   ->
            if message="" then
                return! loop() 
            printfn "%s" ""
            printfn "[message received] %s" message
            let result = message.Split ','
            let mutable req= result.[0]
            let mutable POST=result.[1]
            let mutable user_name=result.[2]
            let mutable pwd=result.[3]
            let mutable to_user=result.[4]
            let mutable message_string=result.[5]
            let mutable hash_mentioned=result.[6]
            let mutable at=result.[7]
            let mutable register=result.[8]
            let mutable task = akka_REG <? Bundle1("","","","")
            if req= "reg" then
//                printfn "[Register] user_name:%s pwd: %s" user_name pwd
                task <- akka_REG <? Bundle1(POST,register,user_name,pwd)
            if req= "send" then
//                printfn "[send] user_name:%s pwd: %s message_string: %s" user_name pwd message_string
                task <- akka_SEND <? Bundle2(POST,user_name,pwd,message_string,false)
            if req= "subscribe" then
//                printfn "[subscribe] user_name:%s pwd: %s sub user_name: %s" user_name pwd to_user
                task <- akka_SUBSCRIBE <? Bundle3(POST,user_name,pwd,to_user )
            if req= "retweet" then
//                printfn "[retweet] user_name:%s pwd: %s message_string: %s" user_name pwd message_string
                task <- akka_RETWEET <? Bundle4(POST,user_name,pwd,message_string)
            if req= "querying" then
//                printfn "[querying] user_name:%s pwd: %s" user_name pwd
                task <- akka_QUERYING <? Bundle5(POST,user_name,pwd )
            if req= "#" then
//                printfn "[#Hashtag] %s: " hash_mentioned
                task <- akka_QUERHASHTAG <? Bundle6(POST,hash_mentioned )
            if req= "@" then
//                printfn "[@mention] %s" at
                task <- akka_AT <? Bundle7(POST,at )
            let response = Async.RunSynchronously (task, 1000)
            sender <? response |> ignore
            printfn "[Result]: %s" response
        return! loop()     
    }
    loop ()
let akka_akka_IncomingMessage = spawn system "EchoServer" akka_incomingMsg
akka_akka_IncomingMessage
// once we received a set of string, dispatch to different functional actor
// dispatch was based on the opt.
 <? "" |> ignore
printfn "       Twitter Online      " 

// For function reg
Console.ReadLine() |> ignore
0


