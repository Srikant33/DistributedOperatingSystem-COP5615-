
open Akka
open System
open Akka.Actor
open Akka.FSharp
open Akka.Configuration
open WebSharper
open WebSharper.Sitelets
open global.Suave
open Suave.Web
open WebSharper.Suave
open System.Security.Cryptography
open System.Text
open System.IO
open System
open System.Text.RegularExpressions











// <summary>
// Encrypts data and files using AES CBC/CFB - 128/192/256 bits.
// 
// The encryption and authentication keys 
// are derived from the supplied key/pwd using HKDF/PBKDF2.
// The key can be set either with `SetMasterKey` or with `RandomKeyGen`.
// Encrypted data format: salt[16] + iv[16] + ciphertext[n] + mac[32].
// Ciphertext authenticity is verified with HMAC SHA256.
// 
// CFB is not supported in .NET Core.
// </summary>
// <param name="mode">reqional, the AES mode (CBC or CFB)</param>
// <param name="size">reqional, the key size (128, 192, 256)</param>
// <exception cref="ArgumentException">
// Thrown when mode is not supported or size is invalid.
// </exception>
type AesEncryption(?mode:string, ?size:int) = 
    let modes = Map.empty.Add("CBC", CipherMode.CBC).Add("CFB", CipherMode.CFB)
    let Sizes  = [ 128; 192; 256 ]
    let lengthOfSalt = 16
    let lengthOfIV = 16
    let lengthOfMAC = 32
    let lengthOfMACKey = 32

    let mode = (defaultArg mode "CBC").ToUpper()
    let lengthOfKey = (defaultArg size 128) / 8
    let size = defaultArg size 128
    let mutable masterKey:byte[] = null

    do
        if not (List.exists ((=) size) Sizes ) then
            raise (ArgumentException "Invalid key size!")
        if not (modes.ContainsKey mode) then
            raise (ArgumentException (mode + " is not supported!"))

    // The number of PBKDF2 iterations (applies to pwd based keys).
    member val keyIterations = 20000 with get, set
    // Accepts ans returns base64 encoded data.
    member val base64 = true with get, set

    // <summary>
    // Encrypts data using a master key or the supplied pwd.
    // 
    // The pwd is not required if a master key has been set 
    // (either with `RandomKeyGgen` or with `SetMasterKey`). 
    // If a pwd is supplied, it will be used to create a key with PBKDF2.
    // </summary>
    // <param name="data">The plaintext.</param>
    // <param name="pwd">reqional, the pwd.</param>
    // <returns>Encrypted data (salt + iv + ciphertext + mac).</returns>
    member this.Encrypt(data:byte[], ?pwd:string):byte[] = 
        let iv = this.RandomBytes lengthOfIV
        let salt = this.RandomBytes lengthOfSalt
        try
            let AESkey, macKey = this.Keys(salt, (defaultArg pwd null))

            use cipher = this.Cipher(AESkey, iv)
            use ict = cipher.CreateEncryptor()
            let ciphertext = ict.TransformFinalBlock(data, 0, data.Length)

            let iv_ct = Array.append iv ciphertext
            let mac = this.Sign(iv_ct, macKey)
            let encrypt = Array.append (Array.append salt iv_ct) mac

            if this.base64 then
                Encoding.ASCII.GetBytes (Convert.ToBase64String encrypt)
            else
                encrypt
        with 
            | :? ArgumentException as e -> this.ErrorHandler e; null
            | :? CryptographicException as e -> this.ErrorHandler e; null
    
    // <summary>Encrypts data using a master key or the supplied pwd.</summary>
    // <param name="data">The plaintext.</param>
    // <param name="pwd">reqional, the pwd.</param>
    // <returns>Encrypted data (salt + iv + ciphertext + mac).</returns>
    member this.Encrypt(data:string, ?pwd:string):byte[] = 
        this.Encrypt (Encoding.UTF8.GetBytes(data), (defaultArg pwd null))
    
    // <summary>
    // Decrypts data using a master key or the supplied pwd.
    // 
    // The pwd is not required if a master key has been set 
    // (either with `RandomKeyGgen` or with `SetMasterKey`). 
    // If a pwd is supplied, it will be used to create a key with PBKDF2.
    // </summary>
    // <param name="data">The ciphertext (raw of base46-encoded bytes).</param>
    // <param name="pwd">reqional, the pasword.</param>
    member this.Decrypt(data:byte[], ?pwd:string):byte[] = 
        let mutable data = data
        try
            if this.base64 then 
                data <- Convert.FromBase64String(Encoding.ASCII.GetString data)
            
            let salt = data.[0..lengthOfSalt - 1]
            let iv = data.[lengthOfSalt..lengthOfSalt + lengthOfIV - 1]
            let ciphertext = data.[lengthOfSalt + lengthOfIV..data.Length - lengthOfMAC - 1]
            let mac = data.[data.Length - lengthOfMAC..data.Length - 1]

            let AESkey, macKey = this.Keys(salt, (defaultArg pwd null))
            this.Verify((Array.append iv ciphertext), mac, macKey)

            use cipher = this.Cipher(AESkey, iv)
            use ict = cipher.CreateDecryptor()
            let plaintext = ict.TransformFinalBlock(ciphertext, 0, ciphertext.Length)
            plaintext
        with 
            | :? ArgumentException as e -> this.ErrorHandler e; null
            | :? CryptographicException as e -> this.ErrorHandler e; null
            | :? FormatException as e -> this.ErrorHandler e; null
            | :? IndexOutOfRangeException as e -> this.ErrorHandler e; null
    
    // <summary>Decrypts data using a master key or the supplied pwd.</summary>
    // <param name="data">The ciphertext (raw of base46-encoded bytes).</param>
    // <param name="pwd">reqional, the pasword.</param>
    member this.Decrypt(data:string, ?pwd:string):byte[] = 
        this.Decrypt (Encoding.UTF8.GetBytes (data), (defaultArg pwd null))
    

    // <summary>
    // Encrypts files using a master key or the supplied pwd.
    // 
    // The pwd is not required if a master key has been set 
    // (either with `RandomKeyGgen` or with `SetMasterKey`). 
    // If a pwd is supplied, it will be used to create a key with PBKDF2.
    // The original file is not modified; a new encrypt file is created.   
    // </summary>
    // <param name="path">The file path.</param>
    // <param name="pwd">reqional, the pasword.</param>
    member this.EncryptFile(path:string, ?pwd:string):string = 
        let iv = this.RandomBytes lengthOfIV
        let salt = this.RandomBytes lengthOfSalt
        try
            let newPath = path + ".enc"
            use fs = new FileStream(newPath, FileMode.Create, FileAccess.Write) 
            fs.Write(salt, 0, lengthOfSalt)
            fs.Write(iv, 0, lengthOfIV)

            let AESkey, macKey = this.Keys(salt, (defaultArg pwd null))
            use cipher = this.Cipher(AESkey, iv)
            use ict = cipher.CreateEncryptor()
            use hmac = new HMACSHA256(macKey)
            hmac.TransformBlock(iv, 0, iv.Length, null, 0) |> ignore

            for data, fend in this.FileChunks(path) do
                let mutable ciphertext = Array.create data.Length 0uy

                if fend then
                    ciphertext <- ict.TransformFinalBlock(data, 0, data.Length)
                    hmac.TransformFinalBlock(ciphertext, 0, ciphertext.Length) |> ignore
                else
                    ict.TransformBlock(data, 0, data.Length, ciphertext, 0) |> ignore
                    hmac.TransformBlock(ciphertext, 0, ciphertext.Length, null, 0) |> ignore
                fs.Write(ciphertext, 0, ciphertext.Length)
            
            let mac = hmac.Hash
            fs.Write(mac, 0, mac.Length)
            newPath
        with 
            | :? ArgumentException as e -> this.ErrorHandler e; null
            | :? CryptographicException as e -> this.ErrorHandler e; null
            | :? UnauthorizedAccessException as e -> this.ErrorHandler e; null
            | :? FileNotFoundException as e -> this.ErrorHandler e; null
    
    // <summary>
    // Decrypts files using a master key or the supplied pwd.
    // 
    // The pwd is not required if a master key has been set 
    // (either with `RandomKeyGgen` or with `SetMasterKey`). 
    // If a pwd is supplied, it will be used to create a key with PBKDF2.
    // The original file is not modified; a new decrypted file is created.
    // </summary>
    // <param name="path">The file path.</param>
    // <param name="pwd">reqional, the pasword.</param>
    member this.DecryptFile(path:string, ?pwd:string):string = 
        let salt = Array.create lengthOfSalt 0uy
        let iv = Array.create lengthOfIV 0uy
        let mac = Array.create lengthOfMAC 0uy

        try
            let newPath = Regex.Replace(path, ".enc$", ".dec")
            let fileSize = (int)(new FileInfo(path)).Length
            use fs = new FileStream(path, FileMode.Open, FileAccess.Read)

            fs.Read(salt, 0, lengthOfSalt) |> ignore
            fs.Read(iv, 0, lengthOfIV) |> ignore
            fs.Seek((int64)(fileSize - lengthOfMAC), SeekOrigin.Begin) |> ignore
            fs.Read(mac, 0, lengthOfMAC) |> ignore

            let AESkey, macKey = this.Keys(salt, (defaultArg pwd null))
            this.VerifyFile(path, mac, macKey)
        
            use fs = new FileStream(newPath, FileMode.Create, FileAccess.Write)
            use cipher = this.Cipher(AESkey, iv)
            use ict = cipher.CreateDecryptor()

            for data, fend in this.FileChunks(path, lengthOfSalt + lengthOfIV, lengthOfMAC) do
                let mutable plaintext = Array.create data.Length 0uy
                let mutable size = 0

                if fend then
                    plaintext <- ict.TransformFinalBlock(data, 0, data.Length)
                    size <- plaintext.Length
                else
                    size <- ict.TransformBlock(data, 0, data.Length, plaintext, 0)
                fs.Write(plaintext, 0, size)
            newPath
        with 
            | :? ArgumentException as e -> this.ErrorHandler e; null
            | :? CryptographicException as e -> this.ErrorHandler e; null
            | :? UnauthorizedAccessException as e -> this.ErrorHandler e; null
            | :? FileNotFoundException as e -> this.ErrorHandler e; null
    
    // <summary>
    // Sets a new master key.
    // This key will be used to create the encryption and authentication keys.
    // </summary>
    // <param name="key">The new master key.</param>
    // <param name="raw">reqional, expexts raw bytes, not base64-encoded.</param>
    member this.SetMasterKey(key:byte[], ?raw:bool) =
        let mutable key = key
        try
            if not (defaultArg raw false) then
                key <- Convert.FromBase64String(Encoding.ASCII.GetString key)
            masterKey <- key
        with 
            | :? FormatException as e -> this.ErrorHandler e
    
    // <summary>
    // Sets a new master key.
    // This key will be used to create the encryption and authentication keys.
    // </summary>
    // <param name="key">The new master key.</param>
    member this.SetMasterKey(key:string) =
        this.SetMasterKey((Encoding.ASCII.GetBytes key), false);

    // <summary>
    // Returns the master key (or null if the key is not set).
    // </summary>
    // <param name="raw">reqional, returns raw bytes, not base64-encoded.</param>
    member this.GetMasterKey(?raw:bool):byte[] =
        if masterKey = null then
            this.ErrorHandler (Exception "The key is not set!")
            null
        elif not (defaultArg raw false) then
            Encoding.ASCII.GetBytes (Convert.ToBase64String masterKey)
        else
            masterKey
    
    // <summary>
    // Generates a new random key.
    // This key will be used to create the encryption and authentication keys.
    // </summary>
    // <param name="lengthOfKey">reqional, the key size.</param>
    // <param name="raw">reqional, returns raw bytes, not base64-encoded.</param>
    member this.RandomKeyGen(?lengthOfKey:int, ?raw:bool):byte[] =
        masterKey <- this.RandomBytes(defaultArg lengthOfKey 32)
        if (defaultArg raw false) then
            masterKey
        else
            Encoding.ASCII.GetBytes (Convert.ToBase64String masterKey)
    
    // Derives encryption and authentication keys from a key or pwd.
    // If the pwd is not null, it will be used to create the keys.
    member private this.Keys(salt:byte[], ?pwd:string) = 
        let pwd = (defaultArg pwd null)
        let mutable dkey:byte[] = null

        if pwd <> null then
            dkey <- this.Pbkdf2Sha512(pwd, salt, lengthOfKey + lengthOfMACKey)
        elif masterKey <> null then
            dkey <- this.HkdfSha256(masterKey, salt, lengthOfKey + lengthOfMACKey)
        else
            raise (ArgumentException "No pwd or key specified!")
        dkey.[..lengthOfKey - 1], dkey.[lengthOfKey..]
    
    // Creates random bytes; used for salt, IV and key generation.
    member private this.RandomBytes(size:int) =
        let rb = Array.create size 0uy
        use rng = new RNGCryptoServiceProvider()
        rng.GetBytes rb
        rb
    
    // Creates an RijndaelManaged object; used for encryption / decryption.
    member private this.Cipher(key:byte[], iv:byte[]):RijndaelManaged =
        let rm =  new RijndaelManaged()
        rm.Mode <- modes.[mode]
        rm.Padding <- if mode = "CFB" then PaddingMode.None else PaddingMode.PKCS7
        rm.FeedbackSize <- if mode = "CFB" then 8 else 128
        rm.KeySize <- size
        rm.Key <- key
        rm.IV <- iv
        rm
    
    // Computes the MAC of ciphertext; used for authentication.
    member private this.Sign(data:byte[], key:byte[]) = 
        use hmac = new HMACSHA256(key)
        hmac.ComputeHash data
    
    // Computes the MAC of ciphertext; used for authentication.
    member private this.SignFile(path:string, key:byte[], ?fstart:int, ?fend:int) = 
        use hmac = new HMACSHA256(key)
        for data, _ in this.FileChunks(path, (defaultArg fstart 0), (defaultArg fend 0)) do 
            hmac.TransformBlock(data, 0, data.Length, null, 0) |> ignore
        hmac.TransformFinalBlock((Array.create 0 0uy), 0, 0) |> ignore
        hmac.Hash
    
    // Verifies the authenticity of ciphertext.
    member private this.Verify(data, mac, key) = 
        let dataMac = this.Sign(data, key)
        if not (this.ConstantTimeComparison (mac, dataMac)) then
            raise (ArgumentException "MAC check failed!")
    
    // Verifies the authenticity of ciphertext.
    member private this.VerifyFile(path:string, mac:byte[], key:byte[]) = 
        let fileMac = this.SignFile(path, key, lengthOfSalt, lengthOfMAC)
        if not (this.ConstantTimeComparison(mac, fileMac)) then
             raise (ArgumentException "MAC check failed!")
    
    // Handles exceptions (prints the exception message by default).  
    member private this.ErrorHandler(e:Exception) =
        printfn "%s" e.Message
    
    // Safely compares two byte arrays, used for uthentication.
    member private this.ConstantTimeComparison(mac1:byte[], mac2:byte[]) =
        let mutable result = mac1.Length ^^^ mac2.Length
        for i in 0 .. (min mac1.Length mac2.Length) - 1 do
            result <- result ||| ((int)mac1.[i] ^^^ (int)mac2.[i])
        result = 0
     
    // A generator that reads a file and yields chunks of data.
    // The chunk size should be a multiple of the block size (16).
    member private this.FileChunks(path:string, ?fbeg:int, ?fend:int):seq<Tuple<byte[], bool>> = 
        let mutable size = 1024
        let fs = new FileStream(path, FileMode.Open, FileAccess.Read)
        let fbeg = defaultArg fbeg 0
        let fend = (int)fs.Length - (defaultArg fend 0)
        let mutable pos = fs.Read(Array.create fbeg 0uy, 0, fbeg)

        seq { while pos < fend do
                size <- if fend - pos > size then size else fend - pos
                let data = Array.create size 0uy
                pos <- pos + fs.Read(data, 0, size)
                yield (data, pos = fend)
        }
    
    // A PBKDF2 algorithm implementation, with HMAC-SHA512.
    member private this.Pbkdf2Sha512(pwd:string, salt:byte[], dlengthOfKey:int):byte[] =
        let mutable dkey = Array.zeroCreate<byte> 0
        use prf = new HMACSHA512(Encoding.UTF8.GetBytes pwd)
        let hashLen = 64;

        for i in 0..hashLen..(dlengthOfKey - 1) do
            let b = Array.rev (BitConverter.GetBytes ((i / hashLen) + 1))
            let mutable u = prf.ComputeHash (Array.append salt b)
            let f = u

            for _ in 1..(this.keyIterations - 1) do
                u <- prf.ComputeHash u
                for k in 0..f.Length - 1 do
                    f.[k] <- f.[k] ^^^ u.[k]
            dkey <- Array.append dkey f
        dkey.[0..dlengthOfKey - 1]
    
    // A PBKHKFDF2 algorithm implementation, with HMAC-SHA256.
    member private this.HkdfSha256(key:byte[], salt:byte[], dlengthOfKey:int):byte[] =
        let mutable dkey = Array.zeroCreate<byte> 0
        let mutable hkey = Array.zeroCreate<byte> 0
        let hashLen = 32;
        use prkHmac = new HMACSHA256(salt)
        let prk = prkHmac.ComputeHash key

        for i in 0..hashLen..(dlengthOfKey - 1) do
            hkey <- Array.append hkey [|(byte (i / hashLen + 1))|]
            use hmac = new HMACSHA256(prk)
            hkey <- hmac.ComputeHash hkey
            dkey <- Array.append dkey hkey
        dkey.[0..dlengthOfKey - 1]




















type EndPoint2 =
    | [<EndPoint "POST /twitter"; Json "body">] Register of body: CMD
and CMD = { command: string}

//let configuration = 
//    ConfigurationFactory.ParseString(
//        @"akka {
//            log-config-on-start : on
//            stdout-loglevel : DEBUG
//            loglevel : ERROR
//            actor {
//                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
//                debug : {
//                    receive : on
//                    autoreceive : on
//                    lifecycle : on
//                    event-stream : on
//                    unhandled : on
//                }
//            }
//            remote {
//                helios.tcp {
//                    port = 8777
//                    hostname = localhost
//                }
//            }
//        }")
//let system = ActorSystem.Create("RemoteFSharp", configuration)
let system = System.create "RemoteFSharp" (Configuration.defaultConfig())


type Tweet(message_id:string, text:string, forward_message:bool) =
    member this.message_id = message_id
    member this.text = text
    member this.forward_message = forward_message
//    member this.time = System.DateTime.Now.ToFileTimeUtc() |> string
    override this.ToString() =
      let mutable res = ""
      if forward_message then
        res <- sprintf "[retweet][%s]%s" this.message_id this.text
      else
        res <- sprintf "[%s]%s" this.message_id this.text
//        res <- sprintf "%s" this.text
      res

//let tweet1 = new Tweet("1", "tweet1", false)
//let tweet2 = new Tweet("2", "tweet2", false)
//let tweet3 = new Tweet("3", "tweet3", false)
//printfn "%A" tweet1

type User(usr:string, pwd:string) =
    let mutable sub = List.empty: User list
    let mutable msg = List.empty: Tweet list
    member this.usr = usr
    member this.pwd = pwd
    member this.addSubscribe x =
        sub <- List.append sub [x]
    member this.getsub() =
        sub
    member this.addTweet x =
        msg <- List.append msg [x]
    member this.getmsg() =
        msg
    override this.ToString() = 
       this.usr
       

//let user1 = new User("user1", "111")
//let user2 = new User("user2", "222")
//let user3 = new User("user3", "333")
//printfn "%A" (user1.usr, user1.pwd, user1.getsub(), user1.getmsg())
//user1.addSubscribe(user1)
//user1.addSubscribe(user3)
//user1.addTweet(tweet1)
//user1.addTweet(tweet3)
//printfn "%A" (user1.usr, user1.pwd, user1.getsub(), user1.getmsg())

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
            res <- "--sorry--, user_name already exist"
        else
            let user = new User(user_name, pwd)
            this.AddUsr user
            user.addSubscribe user
            res <- "Register success user_name: " + user_name + "  pwd: " + pwd
        res
    member this.SendTweet user_name pwd text forward_message =
        let mutable res = ""
        if not (this.authentication user_name pwd) then
            res <- "--sorry--, authentication fail"
        else
            if users.ContainsKey(user_name)=false then
                res <-  "--sorry--, no this user_name"
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
                printfn "%A" "--sorry--, no this user_name"
            else
                let user = users.[user_name]
                if user.pwd = pwd then
                    res <- true
            res
    member this.getUser user_name = 
        let mutable res = new User("","")
        if not (users.ContainsKey(user_name)) then
            printfn "%A" "--sorry--, no this user_name"
        else
            res <- users.[user_name]
        res
    member this.subscribe user_name1 pwd user_name2 =
        let mutable res = ""
        if not (this.authentication user_name1 pwd) then
            res <- "--sorry--, authentication fail"
        else
            let user1 = this.getUser user_name1
            let user2 = this.getUser user_name2
            user1.addSubscribe user2
            res <- "--sent-- " + user_name1 + " subscribe " + user_name2
        res
    member this.reTweet user_name pwd text =
        let res = "[retweet]" + (this.SendTweet user_name pwd text true)
        res
    member this.querymsgSubscribed user_name pwd =
        let mutable res = ""
        if not (this.authentication user_name pwd) then
            res <- "--sorry--, authentication fail"
        else
            let user = this.getUser user_name
            let res1 = user.getsub() |> List.map(fun x-> x.getmsg()) |> List.concat |> List.map(fun x->x.ToString()) |> String.concat "\n"
            res <- "--sent-- querymsgSubscribed" + "\n" + res1
        res
    member this.hash_mentioned hashtag =
        let mutable res = ""
        if not (hashtags.ContainsKey(hashtag)) then
            res <- "--sorry--, no this hashtag"
        else
            let res1 = hashtags.[hashtag] |>  List.map(fun x->x.ToString()) |> String.concat "\n"
            res <- "--sent-- hash_mentioned" + "\n" + res1
        res
    member this.queryMention mention =
        let mutable res = ""
        if not (mentions.ContainsKey(mention)) then
            res <- "--sorry--, no this mention"
        else
            let res1 = mentions.[mention] |>  List.map(fun x->x.ToString()) |> String.concat "\n"
            res <-  "--sent-- queryMention" + "\n" + res1
        res
    override this.ToString() =
        "print the entire Twitter"+ "\n" + msg.ToString() + "\n" + users.ToString() + "\n" + hashtags.ToString() + "\n" + mentions.ToString()
        
    
let twitter = new Twitter()
//printfn "%A" twitter
// test 1
//twitter.AddUsr user1
//twitter.AddUsr user3
//twitter.AddTweet tweet1
//twitter.AddTweet tweet3
//twitter.AddToMention "@Biden" tweet1
//twitter.AddToMention "@Biden" tweet3
//twitter.AddToMention "@Trump" tweet2
//twitter.AddToHashTag "#Singapore" tweet1
//twitter.AddToHashTag "#Singapore" tweet2
//twitter.AddToHashTag "#France" tweet3

// test 2
//twitter.register "user1" "123456"
//printfn ""
//twitter.register "user2" "123456"
//printfn ""
//twitter.SendTweet "user1" "123456" "tweet1 @Biden " false
//printfn ""
//twitter.SendTweet "user2" "123456" "tweet2 #Trump " false
//printfn ""
//twitter.SendTweet "user2" "123456" "tweet3 @Biden #Trump " false
//printfn ""
//twitter.reTweet "user_name" "pwd" "im retwittering..."
//printfn ""
//twitter.subscribe "user1" "123456" "user2"
//printfn ""
//twitter.querymsgSubscribed "user1" "123456"
//printfn ""
//twitter.querymsgSubscribed "user2" "123456"
//printfn ""
//twitter.hash_mentioned "#Trump"
//printfn ""
//twitter.queryMention "@Biden"
//printfn ""
//printfn "%A" twitter

////   The actor and message were defined below     //////

// Define message pack for different actor
// actor reg:  "POST", "/register", "user_name", "pwd"
type Bundle_reg = Bundle1 of  string*string*string* string
// actor send:  "POST", "user_name", "pwd", "tweet_content", false
type Bundle_send = Bundle2 of  string*string*string* string* bool
// actor subscribe:  "POST", "user_name", "pwd","target_user_name" 
type Bundle_subscribe = Bundle3 of  string*string*string* string 
// actor retweet:  "POST", "user_name", "pwd", "tweet_content" 
type Bundle_remsg = Bundle4 of  string*string*string * string
//twitter.querymsgSubscribed "user1" "123456"
// actor querymsgSubscribed:  "POST", "user_name", "pwd"  
type Bundle_querymsgSubscribed = Bundle5 of  string*string*string 
// actor #, hash_mentioned:  "POST", "hash_mentioned" 
type Bundle_hashtag = Bundle6 of  string*string   
// actor @, hash_mentioned:  "POST", "at" 
type Bundle_at = Bundle7 of  string*string  

// The 1st actor is for reg
let akka_reg (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
//        printfn "%A" message
        let sender = mailbox.Sender()
        match message  with
        |   Bundle1(a,register,user_name,pwd) ->
            let res = twitter.register user_name pwd
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()
// The 2nd actor is for send
let akka_send (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        let sender_path = mailbox.Sender().Path.ToStringWithAddress()
        match message with
        |   Bundle2(a,user_name,pwd,tweet_content,false) -> 
            let res = twitter.SendTweet user_name pwd tweet_content false
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()
// The 3rd actor is for subscribe
let akka_subscribe (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle3(a,user_name,pwd,target_user_name) -> 
            let res = twitter.subscribe user_name pwd target_user_name
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()
// The 4th actor is for retweet
let akka_retweet (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle4(a,user_name,pwd,tweet_content) -> 
            let res = twitter.reTweet  user_name pwd tweet_content
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()
// The 5th actor is for query msg Subscribed
let akka_querying (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle5(a,user_name,pwd ) -> 
            let res = twitter.querymsgSubscribed  user_name pwd
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()
// The 6th actor is for hash_mentioned
let akka_hash_mentioned (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle6(a,hash_mentioned) -> 
            let res = twitter.hash_mentioned  hash_mentioned
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()
// The 7th actor is for @ (at)
let akka_at (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
        match message  with
        |   Bundle7(a,at) -> 
            let res = twitter.queryMention  at
            sender <? res |> ignore
        | _ ->  failwith "unknown message"
        return! loop()     
    }
    loop ()

//////////////////
// req is the operation we will use
// req= "reg", "send", "subscribe", "retweet", "querying", "#" , "@"
let mutable req= "reg" 
//let mutable POST="POST"
let mutable user_name="user2"
let mutable pwd="123456"
//let mutable register="register"
let mutable target_user_name="user1"
let mutable tweet_content="Today is a good day!"
let mutable hash_mentioned="#Trump"
let mutable at="@Biden"
// Bundle between akka_P defined below:
//( req,POST,user_name,pwd,target_user_name,tweet_content,hash_mentioned,at,register)
type Bundle_akka_P = Bundle8 of  string*string * string* string* string * string* string* string * string
//Bundle8( req,POST,user_name,pwd,target_user_name,tweet_content,hash_mentioned,at,register)
// dedined the message received actor


let akka_REG = spawn system "akka_P1" akka_reg
let akka_SEND = spawn system "akka_P2" akka_send
let akka_SUBSCRIBE = spawn system "akka_P3" akka_subscribe
let akka_RETWEET = spawn system "akka_P4" akka_retweet
let akka_QUERYING = spawn system "akka_P5" akka_querying 
let akka_QUERHASHTAG = spawn system "akka_P6" akka_hash_mentioned
let akka_AT = spawn system "akka_P7" akka_at

// for the server, received string and dispatch
let akka_incomingMsg (mailbox: Actor<_>) = 
    let rec loop () = actor {        
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender()
//        printfn "%s" message
        match box message with
        | :? string   ->
            if message="" then
                return! loop() 
            //(req,POST,user_name,pwd,target_user_name,tweet_content,hash_mentioned,at,register)
            printfn "%s" ""
            printfn "[message received] %s" message
            let result = message.Split ','
            let mutable req= result.[0]
            let mutable POST=result.[1]
            let mutable user_name=result.[2]
            let mutable pwd=result.[3]
            let mutable target_user_name=result.[4]
            let mutable tweet_content=result.[5]
            let mutable hash_mentioned=result.[6]
            let mutable at=result.[7]
            let mutable register=result.[8]
            let mutable task = akka_REG <? Bundle1("","","","")
            // For function reg
            if req= "reg" then
                printfn "[Register] user_name:%s pwd: %s" user_name pwd
                task <- akka_REG <? Bundle1(POST,register,user_name,pwd)
            // For function send
            if req= "send" then
                printfn "[send] user_name:%s pwd: %s tweet_content: %s" user_name pwd tweet_content
                task <- akka_SEND <? Bundle2(POST,user_name,pwd,tweet_content,false)
            // For function subscribe
            if req= "subscribe" then
                printfn "[subscribe] user_name:%s pwd: %s sub user_name: %s" user_name pwd target_user_name
                task <- akka_SUBSCRIBE <? Bundle3(POST,user_name,pwd,target_user_name )
            // For function retweet
            if req= "retweet" then
                printfn "[retweet] user_name:%s pwd: %s tweet_content: %s" user_name pwd tweet_content
                task <- akka_RETWEET <? Bundle4(POST,user_name,pwd,tweet_content)
            // For function retweet
            if req= "querying" then
                printfn "[querying] user_name:%s pwd: %s" user_name pwd
                task <- akka_QUERYING <? Bundle5(POST,user_name,pwd )
            // For function retweet hash_mentioned
            if req= "#" then
                printfn "[#Hashtag] %s: " hash_mentioned
                task <- akka_QUERHASHTAG <? Bundle6(POST,hash_mentioned )
            // For function @
            if req= "@" then
                printfn "[@mention] %s" at
                task <- akka_AT <? Bundle7(POST,at )
            let response = Async.RunSynchronously (task, 1000)
            sender <? response |> ignore
            printfn "[Result]: %s" response
        return! loop()     
    }
    loop ()
let akka_IncomingMessage = spawn system "EchoServer" akka_incomingMsg

let Aes = new AesEncryption("cbc", 256)

let decrypt (Cipherstring:string) = 
    let Cipherstring2=Encoding.UTF8.GetBytes Cipherstring
    let Plaintext = Aes.Decrypt(Cipherstring2, " ")
    let Plainstring =Encoding.UTF8.GetString Plaintext
    Plainstring

[<Website>]
let Main =

    let mainWebsite = Application.MultiPage (fun context action ->
        match action with
        | EndPoint2.Register body ->
            // TODO de-encryption:     body.command (encrypt string) -> string(,,,,,,,)
            let decrypted_string = decrypt body.command
            printfn "[Before decrypted:]%s" body.command
            printfn "[decrypted Message:]%s" decrypted_string
            let task = akka_IncomingMessage <? decrypted_string
            let response = Async.RunSynchronously (task, 1000)
            Content.Text response
    )

    Sitelet.Sum [ mainWebsite ]

//akka_IncomingMessage <? "" |> ignore



[<EntryPoint>] 
let main argv =
    printfn "%A" argv
    // once we received a set of string, dispatch to different functional actor
    // dispatch was based on the req.
    
//    akka_IncomingMessage <? "" |> ignore
    printfn "*     *     *     *     *     *     *     *     *     *     *     *     * \n " 
    printfn "*     *     *     *     *     *     *     *     *     *     *     *     *   " 
    printfn "Twitter Server is running...   " 
    printfn "*     *     *     *     *     *     *     *     *     *     *     *     *   "
    
    // TODO Very important: This sentence must be inside the Main, otherwise it will not work
    startWebServer defaultConfig
        (WebSharperAdapter.ToWebPart(Main, RootDirectory="../.."))
    
    // For function reg
    Console.ReadLine() |> ignore
   
    printfn "*     *     *     *     *     *     *     *     *     *     *     *     *----------\n" 
    0





















































    open System
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open System.IO
open System
open System.Threading
open Akka.Actor
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
type Bundle_akka_P = Bundle8 of  string*string * string* string* string * string* string* string * string

// number of user
let N = 10
let M = N