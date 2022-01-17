#r "nuget: Akka" 
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 

open System
open Akka.Actor
open Akka.FSharp

let args : string array = fsi.CommandLineArgs |> Array.tail

let system = System.create "processor" <| Configuration.load ()
let runTime = System.Diagnostics.Stopwatch.StartNew()
let  N=args.[0] |> int // number of actors
let max_count=10
let topo= args.[1] // topo= "full", topo= "3D", topo= "line", topo= "imp3D" 
let algorithm = args.[2] // "gossip" or "push-sum"

let cbrt= Convert.ToInt32( round (Math.Cbrt (double N)))
let new_N_small_c=(cbrt)*(cbrt)*(cbrt)
let new_N_large_c=(cbrt+1)*(cbrt+1)*(cbrt+1)
let mutable new_N_c =(cbrt)*(cbrt)*(cbrt)

// upper floor to turn up N into next whole cube number
if N <= new_N_small_c
    then 
        new_N_c<-new_N_small_c
        printfn "Your N is:  %i, N cuberoot is: %i, turned up N is: %i" N cbrt new_N_c
    else
        new_N_c<-new_N_large_c
        printfn "Your N is:  %i, N cuberoot is: %i, turned up N is: %i" N cbrt new_N_c

let mutable m =Convert.ToInt32( round (Math.Cbrt (double new_N_c)))

let mutable array_terminate = [| for i in 1.. N -> 0 |]
let mutable array_terminate_count = [| for i in 1.. N -> 0 |]
let mutable array_terminate_sum = array_terminate|> Array.sum

let  mutable array_s = [| for i in 1.. N ->(double i) |]
let  mutable array_w = [| for i in 1.. N -> (double 1) |]
let  mutable array_r = [| for i in 1.. N -> (double i) |]


let  mutable array_terminate_new_N_c = [| for i in 1.. N -> 0 |] // 0-1
let  mutable array_terminate_count_new_N_c = [| for i in 1.. new_N_c -> 0 |] //0-3
let mutable array_terminate_sum_new_N_c = array_terminate_new_N_c|> Array.sum

// initiating array s, w and r=s/w   Ratio
let  mutable array_s_new_N_c = [| for i in 1.. new_N_c ->(double i) |]
let  mutable array_w_new_N_c = [| for i in 1.. new_N_c -> (double 1) |]
let  mutable array_r_new_N_c = [| for i in 1.. new_N_c -> (double i) |]

//type ProcessorMessage = ProcessJob of string * int * string
//"Rumor" * target num * "topo type" = string * int * string
type ProcessorMessage = ProcessJob of  string * int * string
type ProcessorMessage1 = ProcessJob1 of   int * string * double * double  
let rand = System.Random()

// gossip_full is for "Full_Network"
let gossip_full (mailbox: Actor<_>) = 
    let rec loop (count) = actor {
        array_terminate_sum <- array_terminate|> Array.sum
        if array_terminate_sum >=N-1
            then
                runTime.Stop()
                else
                    let! message = mailbox.Receive ()
                    let sender = mailbox.Sender()
                    let sender_path = mailbox.Sender().Path.ToStringWithAddress()
                    match message  with
                    |   ProcessJob(x,num,topo) ->
                        let mutable rand_num = Random( ).Next() % N
                        while array_terminate.[rand_num]=1  do
                            rand_num <- Random( ).Next() % N
                        while rand_num = num  do
                            rand_num <- Random( ).Next() % N

                        if (count+1) >= max_count 
                            then  
                                array_terminate.[num]<-1
                                if array_terminate_sum <N
                                    then
                                        let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                        gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                            else 
                                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                    return! loop(count+1)     
    }
    loop 0

// gossip_3D is for "3D_Grid"
let gossip_3D (mailbox: Actor<_>) = 
    let rec loop (count) = actor {
        array_terminate_sum_new_N_c  <- array_terminate_new_N_c |> Array.sum
        if array_terminate_sum_new_N_c  >= N-1
            then
                runTime.Stop()
            else
                let! message = mailbox.Receive ()
                let sender = mailbox.Sender()
                let sender_path = mailbox.Sender().Path.ToStringWithAddress()
                match message  with
                |   ProcessJob(x,num,topo) ->
                    let mutable direction=Random( ).Next() % 6
                    let mutable rand_num = num
                    let mutable edge = 1 |>int
                    let mutable last= 1 |>int 
                    let mutable temp = rand_num

                    while last <> 0 do
                        last <- 0
                        edge <- 1
                        temp<-rand_num
                        while edge <> 0 do 
                            direction <- Random( ).Next() % 6
                            edge <- 0
                            if rand_num%m= 0 && direction =3 
                                then 
                                    edge <- 1
                                elif 
                                    rand_num%m=m-1 && direction =1 then edge <- 1

                            if rand_num%(m*m) < m && direction=0 
                                then
                                    edge <- 1
                                elif 
                                    rand_num%(m*m) >= (m*m)-m && direction =2 then edge <- 1

                            if rand_num < m*m && direction = 5 
                                then 
                                    edge <- 1
                                elif 
                                    rand_num >= (m*m*m)-(m*m) && direction =4 then edge <- 1
                        
                        if direction=0 then
                            rand_num<- num-m 
                            // printf " move back "
                            elif direction=1 then 
                                rand_num<- num+1
                                // printf " move right "
                            elif direction=2 then 
                                rand_num<- num+m
                                //printf " move forward "
                            elif direction=3 then
                                rand_num<- num-1
                                //printf " move left "
                            elif direction=4 then 
                                rand_num<- num+(m*m)
                                //printf " move up "
                            elif direction=5 then 
                                rand_num<- num-(m*m)
                                //printf " move down "
                        
                        if rand_num > N
                            then 
                                last <- 1
                                rand_num <- temp
                    
                    if (count+1) >= max_count 
                        then  
                            array_terminate_new_N_c .[num]<-1
                            if array_terminate_sum_new_N_c <new_N_c
                                then
                                    let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                    gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                        else 
                            let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                            gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                return! loop(count+1)     
    }
    loop 0

// gossip_imp3D is for "Imperfect 3D Grid:"
let gossip_imp3D (mailbox: Actor<_>) = 
    let rec loop (count) = actor {
        array_terminate_sum_new_N_c  <- array_terminate_new_N_c |> Array.sum
        if array_terminate_sum_new_N_c  >=new_N_c-1
            then
                runTime.Stop()
                else
                    let! message = mailbox.Receive ()
                    let sender = mailbox.Sender()
                    let sender_path = mailbox.Sender().Path.ToStringWithAddress()
                    match message  with
                    |   ProcessJob(x,num,topo) ->
                        let mutable direction=Random( ).Next() % 7
                        let mutable rand_num = num
                        let mutable edge = 1 |>int
                        let mutable last= 1 |>int 
                        let mutable temp = rand_num

                        while last <> 0 do
                            last <- 0
                            edge <- 1
                            temp<-rand_num
                            while edge <> 0 do 
                                direction <- Random( ).Next() % 7
                                edge <- 0
                                if rand_num%m= 0 && direction =3 
                                    then 
                                        edge <- 1
                                    elif 
                                        rand_num%m=m-1 && direction =1 then edge <- 1

                                if rand_num%(m*m) < m && direction=0 
                                    then
                                        edge <- 1
                                    elif 
                                        rand_num%(m*m) >= (m*m)-m && direction =2 then edge <- 1

                                if rand_num < m*m && direction = 5 
                                    then 
                                        edge <- 1
                                    elif 
                                        rand_num >= (m*m*m)-(m*m) && direction =4 then edge <- 1
                                                   
                            if direction=0 then
                                rand_num<- num-m 
                                // printf " move back "
                                elif direction=1 then 
                                    rand_num<- num+1
                                    // printf " move right "
                                elif direction=2 then 
                                    rand_num<- num+m
                                    //printf " move forward "
                                elif direction=3 then
                                    rand_num<- num-1
                                    //printf " move left "
                                elif direction=4 then 
                                    rand_num<- num+(m*m)
                                    //printf " move up "
                                elif direction=5 then 
                                    rand_num<- num-(m*m)
                                    //printf " move down "
                                elif direction=6 then
                                    rand_num <- Random( ).Next() % N
                                    //printf "move randomly"
                            if rand_num > N
                                then 
                                    last <- 1
                                    rand_num <- temp
                        
                        if (count+1) >= max_count 
                            then  
                                array_terminate_new_N_c .[num]<-1
                                if array_terminate_sum_new_N_c <new_N_c
                                    then
                                        let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                        gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                            else 
                                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                    return! loop(count+1)     
    }
    loop 0

// gossip_line is for "line"
let gossip_line (mailbox: Actor<_>) = 
    let rec loop (count) = actor {
        array_terminate_sum <- array_terminate|> Array.sum
        if array_terminate_sum >= N-1
            then
                runTime.Stop()
                else
                    let! message = mailbox.Receive ()
                    let sender = mailbox.Sender()
                    let sender_path = mailbox.Sender().Path.ToStringWithAddress()
                    match message  with
                    |   ProcessJob(x,num,topo) ->                    
                        let mutable direction=0
                        let mutable rand_num = num
                        direction<-Random( ).Next() % 2 
                        if direction=1 then rand_num<- num+1
                        if direction=0 then rand_num<- num-1
                        
                        
                        if array_terminate_sum>=N-cbrt then
                            rand_num<-Random( ).Next() % N                           
                        if rand_num=1 then rand_num<-num+1
                        if rand_num=N then rand_num<-num-1
                            

                        if (count+1) >= max_count 
                            then  
                                array_terminate.[num]<-1
                                if array_terminate_sum <N
                                    then
                                        let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                        gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                            else 
                                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                                gossip_spread <? ProcessJob( "Roumor",rand_num,topo)|>ignore
                    return! loop(count+1)     
    }
    loop 0

// pushSum
let pushSum_full (mailbox: Actor<_>) = 
    let rec loop (count ) = actor {
        let! message = mailbox.Receive ()
        match message  with
        |   ProcessJob1(num,topo,s,w ) ->
            // the (s, w)   are sender's current number in global array
            // add it to receiver's (s, w)
            array_terminate_sum <- array_terminate|> Array.sum
            if array_terminate_sum >= N-1 then runTime.Stop()
            // finding next hop
            let mutable rand_num = Random( ).Next() % N
            // updating (s,w)
            array_s.[num]<-array_s.[num]+s
            array_w.[num]<-array_w.[num]+w
            // updating ratio
            let mutable new_r=double(s/w)
            let mutable e= abs(double(new_r-array_r.[num]))
            // stop criteria for error
            if e < 1e-10 then array_terminate_count.[num]<-array_terminate_count.[num]+1
            // stop criteria for actormore than max times
            if array_terminate_count.[num] >= max_count
                then 
                    //flag=1
                    array_terminate.[num]<- 1
                    // when it is terminated
                    array_terminate_sum <- array_terminate|> Array.sum
                    if array_terminate_sum <N 
                        then
                            // redirecting, cut itself half and send another half
                            array_s.[num]<-array_s.[num]/2.0
                            array_w.[num]<-array_w.[num]/2.0
                            array_r.[num]<-array_s.[num]/array_w.[num]
                            // printfn "Still have actor not Ternimated, I'm (%i), redirecting to： (%i)" num rand_num
                            let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                            gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
                            
            else
                array_s.[num]<-array_s.[num]/2.0
                array_w.[num]<-array_w.[num]/2.0
                array_r.[num]<-array_s.[num]/array_w.[num]
                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
        return! loop(count+1)        
    }
    loop 0
        

// pushSum_3D is for "3D_Grid"
let pushSum_3D (mailbox: Actor<_>) = 
    //let mutable count = 0
    let rec loop (count ) = actor {
        let! message = mailbox.Receive ()
        match message  with
        |   ProcessJob1(num,topo,s,w ) ->
            array_terminate_sum <- array_terminate|> Array.sum
            if array_terminate_sum >= N-1 then runTime.Stop()
           
            let mutable direction=Random( ).Next() % 6
            let mutable rand_num = num
            let mutable edge = 1 |>int
            let mutable last= 1 |>int 
            let mutable temp = rand_num

            while last <> 0 do
                last <- 0
                edge <- 1
                temp<-rand_num
                while edge <> 0 do 
                    direction <- Random( ).Next() % 6
                    edge <- 0
                    if rand_num%m= 0 && direction =3 
                        then 
                            edge <- 1
                        elif 
                            rand_num%m=m-1 && direction =1 then edge <- 1

                    if rand_num%(m*m) < m && direction=0 
                        then
                            edge <- 1
                        elif 
                            rand_num%(m*m) >= (m*m)-m && direction =2 then edge <- 1

                    if rand_num < m*m && direction = 5 
                        then 
                            edge <- 1
                        elif 
                            rand_num >= (m*m*m)-(m*m) && direction =4 then edge <- 1
                
                if direction=0 then
                    rand_num<- num-m 
                    //printf " move back "
                    elif direction=1 then 
                        rand_num<- num+1
                        //printf " move right "
                    elif direction=2 then 
                        rand_num<- num+m
                        //printf " move forward "
                    elif direction=3 then
                        rand_num<- num-1
                        //printf " move left "
                    elif direction=4 then 
                        rand_num<- num+(m*m)
                        //printf " move up "
                    elif direction=5 then 
                        rand_num<- num-(m*m)
                        //printf " move down "
                
                if rand_num > N
                    then 
                        last <- 1
                        rand_num <- temp
           
            if array_terminate_sum>=N-cbrt then
                rand_num<-Random( ).Next() % N      
            if rand_num<0 then rand_num<-num+1
            if rand_num>=N then rand_num<-num-m
            // updating (s,w)
            array_s.[num]<-array_s.[num]+s
            array_w.[num]<-array_w.[num]+w
            // updating ratio
            let mutable new_r=double(s/w)
            let mutable e= abs(double(new_r-array_r.[num]))
            // stop criteria for error
            if e < 1e-10 then array_terminate_count.[num]<-array_terminate_count.[num]+1
            // stop criteria for actor: 3 times
            if array_terminate_count.[num] >= max_count
                then 
                    //flag=1
                    array_terminate.[num]<- 1
                    // when its terminated
                    array_terminate_sum <- array_terminate|> Array.sum
                    if array_terminate_sum <N 
                        then
                            // redirecting, cut itself half and send another half
                            array_s.[num]<-array_s.[num]/2.0
                            array_w.[num]<-array_w.[num]/2.0
                            array_r.[num]<-array_s.[num]/array_w.[num]
                            let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                            gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
                            
            else
                array_s.[num]<-array_s.[num]/2.0
                array_w.[num]<-array_w.[num]/2.0
                array_r.[num]<-array_s.[num]/array_w.[num]
                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
        return! loop(count+1)        
    }
    loop 0

// pushSum_imp3D is for "Imperfect 3D Grid:"
let pushSum_imp3D (mailbox: Actor<_>) = 
    //let mutable count = 0
    let rec loop (count ) = actor {
        let! message = mailbox.Receive ()
        match message  with
        |   ProcessJob1(num,topo,s,w ) ->
            array_terminate_sum <- array_terminate|> Array.sum
            if array_terminate_sum >= N-1 then runTime.Stop()
            let mutable direction=Random( ).Next() % 7
            let mutable rand_num = num
            let mutable edge = 1 |>int
            let mutable last= 1 |>int 
            let mutable temp = rand_num

            while last <> 0 do
                last <- 0
                edge <- 1
                temp<-rand_num
                while edge <> 0 do 
                    direction <- Random( ).Next() % 7
                    edge <- 0
                    if rand_num%m= 0 && direction =3 
                        then 
                            edge <- 1
                        elif 
                            rand_num%m=m-1 && direction =1 then edge <- 1

                    if rand_num%(m*m) < m && direction=0 
                        then
                            edge <- 1
                        elif 
                            rand_num%(m*m) >= (m*m)-m && direction =2 then edge <- 1

                    if rand_num < m*m && direction = 5 
                        then 
                            edge <- 1
                        elif 
                            rand_num >= (m*m*m)-(m*m) && direction =4 then edge <- 1
                
                if direction=0 then
                    rand_num<- num-m 
                    // printf " move back "
                    elif direction=1 then 
                        rand_num<- num+1
                        // printf " move right "
                    elif direction=2 then 
                        rand_num<- num+m
                        //printf " move forward "
                    elif direction=3 then
                        rand_num<- num-1
                        //printf " move left "
                    elif direction=4 then 
                        rand_num<- num+(m*m)
                        //printf " move up "
                    elif direction=5 then 
                        rand_num<- num-(m*m)
                        //printf " move down "
                    elif direction=6 then
                        rand_num <- Random( ).Next() % N
                        // printf "move randomly"
                if rand_num > N
                    then 
                        last <- 1
                        rand_num <- temp
            
            if array_terminate_sum>=N-cbrt then
                rand_num<-Random( ).Next() % N 
            let mutable randomdice=Random( ).Next() % 7     //6 directions and one random value
            if randomdice=1 then rand_num<-Random( ).Next() % N
              
            if rand_num<0 then rand_num<-num+1
            if rand_num>=N then rand_num<-num-(m*m)
            array_s.[num]<-array_s.[num]+s
            array_w.[num]<-array_w.[num]+w
            let mutable new_r=double(s/w)
            let mutable e= abs(double(new_r-array_r.[num]))
            if e < 1e-10 then array_terminate_count.[num]<-array_terminate_count.[num]+1
            if array_terminate_count.[num] >= max_count
                then 
                    //flag=1
                    array_terminate.[num]<- 1
                    // maybe need redirecting even itself is terminated
                    array_terminate_sum <- array_terminate|> Array.sum
                    if array_terminate_sum <N 
                        then
                            // redirecting, cut itself half and send another half
                            array_s.[num]<-array_s.[num]/2.0
                            array_w.[num]<-array_w.[num]/2.0
                            array_r.[num]<-array_s.[num]/array_w.[num]
                            // printfn "Still have actor not Ternimated, I'm (%i), redirecting to： (%i)" num rand_num
                            let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                            gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
                            
            else
                array_s.[num]<-array_s.[num]/2.0
                array_w.[num]<-array_w.[num]/2.0
                array_r.[num]<-array_s.[num]/array_w.[num]
                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
        return! loop(count+1)        
    }
    loop 0

// pushSum_line is for "line"
let pushSum_line (mailbox: Actor<_>) = 
    let rec loop (count ) = actor {
        let! message = mailbox.Receive ()
        match message  with
        |   ProcessJob1(num,topo,s,w ) ->
            array_terminate_sum <- array_terminate|> Array.sum
            if array_terminate_sum >= N-1 then runTime.Stop()
            let mutable direction=0
            let mutable rand_num = num
            direction<-Random( ).Next() % 2 
            if direction=1 then rand_num<- num+1
            if direction=0 then rand_num<- num-1
                        
            if array_terminate_sum>=N-cbrt then
                rand_num<-Random( ).Next() % N      
            if rand_num<0 then rand_num<-num+1
            if rand_num>=N then rand_num<-num-1
            array_s.[num]<-array_s.[num]+s
            array_w.[num]<-array_w.[num]+w
            // updating ratio
            let mutable new_r=double(s/w)
            let mutable e= abs(double(new_r-array_r.[num]))
            // stop criteria for error
            if e < 1e-10 then array_terminate_count.[num]<-array_terminate_count.[num]+1
            // stop criteria for actor for the number of times it needs to be traversed
            if array_terminate_count.[num] >= max_count
                then 
                    array_terminate.[num]<- 1
                    // when it is terminated
                    array_terminate_sum <- array_terminate|> Array.sum
                    if array_terminate_sum <N 
                        then
                            // redirecting, cut itself half and send another half
                            array_s.[num]<-array_s.[num]/2.0
                            array_w.[num]<-array_w.[num]/2.0
                            array_r.[num]<-array_s.[num]/array_w.[num]
                            let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                            gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
                            
            else
                array_s.[num]<-array_s.[num]/2.0
                array_w.[num]<-array_w.[num]/2.0
                array_r.[num]<-array_s.[num]/array_w.[num]
                let gossip_spread = system.ActorSelection("akka://processor/user/" + string rand_num)
                gossip_spread <? ProcessJob1(rand_num,topo,array_s.[num],array_w.[num])|>ignore
        return! loop(count+1)        
    }
    loop 0

if algorithm="gossip" then
    if topo="full" 
        then
            let actorArray = Array.create N (spawn system "gossip_full" gossip_full)
            {0..N-1} |> Seq.iter (fun a ->
                actorArray.[a] <- spawn system (string a) gossip_full
            )
            actorArray.[1] <? ProcessJob("Roumor",2,topo)|>ignore
            let System_wait=0
            while array_terminate_sum < N-1 do
                System_wait|>ignore
            printfn "-----------------------------------------------------------------------------\n" 
            printf "Only one actor alive so it can't gossip to itself. \n"
            printf "No more spreading available. Algorithm terminating... \n" 
            printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum) N
            printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds

    if topo= "3D"
        then
            printfn "Your N is:  %i, turned up N is: %i" N  new_N_c
            let actorArray = Array.create new_N_c (spawn system "gossip_3D" gossip_3D)
            {0..new_N_c-1} |> Seq.iter (fun a ->
                actorArray.[a] <- spawn system (string a) gossip_3D
            )
            actorArray.[1] <? ProcessJob("Roumor",2,topo)|>ignore
            let System_wait=0
            while array_terminate_sum_new_N_c < N-2 do
                System_wait|>ignore
            printfn "-----------------------------------------------------------------------------\n" 
            printf "Only two actors alive which will cost too much time to find random path. \n"
            printf "No more spreading available. Algorithm terminating... \n" 
            printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum_new_N_c) new_N_c
            printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds

    if topo= "imp3D"
    then
        printfn "Your N is:  %i, turned up N is: %i" N  new_N_c
        //printf "For 2D topo, the N you enter will turn up into next square"
        let actorArray = Array.create new_N_c (spawn system "gossip_imp3D" gossip_imp3D)
        {0..new_N_c-1} |> Seq.iter (fun a ->
            actorArray.[a] <- spawn system (string a) gossip_imp3D
        )
        actorArray.[1] <? ProcessJob("Roumor",2,topo)|>ignore
        let System_wait=0
        while array_terminate_sum_new_N_c < new_N_c-2 do
            System_wait|>ignore
        printfn "-----------------------------------------------------------------------------\n" 
        printf "Only two actors alive which will cost too much time to find random path. \n"
        printf "No more spreading available. Algorithm terminating... \n" 
        printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum_new_N_c) new_N_c
        printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds
            

    if topo="line" then
        let actorArray = Array.create N (spawn system "gossip_line" gossip_line)
        {0..N-1} |> Seq.iter (fun a ->
            actorArray.[a] <- spawn system (string a) gossip_line
        )
        actorArray.[1] <? ProcessJob("Roumor",2,topo)|>ignore
        let System_wait=0
        while array_terminate_sum < N-1 do
            System_wait|>ignore
        printfn "-----------------------------------------------------------------------------\n" 
        printf "Only two actors alive which will cost too much time to find random path. \n"
        printf "No more spreading available. Algorithm terminating... \n" 
        printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum) N
        printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds

if algorithm="push-sum" then
    if topo="full" 
        then
            let actorArray = Array.create N (spawn system "pushSum_full" pushSum_full)
            {0..N-1} |> Seq.iter (fun a ->
                actorArray.[a] <- spawn system (string a) pushSum_full
            )
            array_s.[1]<-array_s.[1]/2.0
            array_w.[1]<-array_w.[1]/2.0
            actorArray.[1] <? ProcessJob1(2,topo,array_s.[1],array_w.[1] )|>ignore
            let System_wait=0
            array_terminate_sum <- array_terminate|> Array.sum
            while array_terminate_sum < N do
                System_wait|>ignore
            printfn "-----------------------------------------------------------------------------\n" 
            printf "No more spreading available.\n" 
            printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum) N
            printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds

    if topo="3D" 
    then
        let actorArray = Array.create N (spawn system "pushSum_3D" pushSum_3D)
        {0..N-1} |> Seq.iter (fun a ->
            actorArray.[a] <- spawn system (string a) pushSum_3D
        )
        array_s.[1]<-array_s.[1]/2.0
        array_w.[1]<-array_w.[1]/2.0
        actorArray.[1] <? ProcessJob1(2,topo,array_s.[1],array_w.[1] )|>ignore
        let System_wait=0
        array_terminate_sum <- array_terminate|> Array.sum
        while array_terminate_sum < N do
            System_wait|>ignore
        printfn "-----------------------------------------------------------------------------\n" 
        printf "No more spreading available.\n" 
        printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum) N
        printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds
    
    if topo="imp3D" 
    then
        let actorArray = Array.create N (spawn system "pushSum_imp3D" pushSum_imp3D)
        {0..N-1} |> Seq.iter (fun a ->
            actorArray.[a] <- spawn system (string a) pushSum_imp3D
        )
        array_s.[1]<-array_s.[1]/2.0
        array_w.[1]<-array_w.[1]/2.0
        actorArray.[1] <? ProcessJob1(2,topo,array_s.[1],array_w.[1] )|>ignore
        let System_wait=0
        array_terminate_sum <- array_terminate|> Array.sum
        while array_terminate_sum < N do
            System_wait|>ignore
        printfn "-----------------------------------------------------------------------------\n" 
        printf "No more spreading available.\n" 
        printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum) N
        printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds
    
    if topo="line" 
    then
        let actorArray = Array.create N (spawn system "pushSum_line" pushSum_line)
        {0..N-1} |> Seq.iter (fun a ->
            actorArray.[a] <- spawn system (string a) pushSum_line
        )
        array_s.[1]<-array_s.[1]/2.0
        array_w.[1]<-array_w.[1]/2.0
        actorArray.[1] <? ProcessJob1(2,topo,array_s.[1],array_w.[1] )|>ignore
        let System_wait=0
        array_terminate_sum <- array_terminate|> Array.sum
        while array_terminate_sum < N do
            System_wait|>ignore
        printfn "-----------------------------------------------------------------------------\n" 
        printf "No more spreading available.\n" 
        printf "ALL STOPPED! Terminated/Total: (%i)/(%i) \n " (array_terminate_sum) N
        printfn "Running time is: %f ms" runTime.Elapsed.TotalMilliseconds

system.WhenTerminated.Wait()