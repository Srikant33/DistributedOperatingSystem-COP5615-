#Distributed Operating System
#Twitter clone and Simulation
#Project 4 Part I
Team Members:
Kasiviswanathan Srikant Iyer (UFID: 5222-2519)
Swaathi Reena Velavan (UFID: 1230-8520

Description:
Build Twitter Engine and add simulation to test it.
1. Implementation
Client:
Akka.actor is used extensively to develop client.fsx. Client sends messages to server,
and receives the response and displays the output.
Server
Server is used to implement the following functionalities:
User:
We can register the user, sign in(active), authenticate and sign out(inactive). Each user has a
username and password.
Tweet:
Each user can send a tweet. Time stamp is added to the instance of the tweet.
Twitter:
This singleton instance consists of multiple hashmaps that help the user search for hashtags
and mentions.
HashTag:
For every tweet, we parse the tweet to find the word starting with # and add it to the hash map.
Mentions:
For every tweet, we parse the tweet to find the word starting with @ and add it to thehash map.
Display Tweets:
Every second, the actor calls itself to check if there was any new tweet added. If so, it displays
the tweet.
2. Run the code
Terminal1: Server: dotnet fsi --langversion:preview Server.fsx
Terminal2:
Client: dotnet fsi --langversion:preview Auto.fsx N
Where N is number of users
3. Output
Terminal1: Server: dotnet fsi --langversion:preview Server.fsx
Terminal2:
dotnet fsi --langversion:preview AUTO.fsx 20000
result10.txt is Performance test result under 20000 users
4. Result
We measure the time taken for each of the following for N number of users:
- Time to register N users
- Time taken for each user to send 10 tweets
- Zipf Subscribe
- Time to check N hashtags
- Time to check N mentions
- Time to perform N random operations
Zipf Subscribe vs Number of Users
4.Largest network of users built: 20000
Result:
Register 20000 users is 21309.168300
Send 10 tweets is 21309.168300
Zipf subscribe 20000 users is 21309.168300
Query 20000 users is 21309.168300
Query 20000 hasgtag is 21309.168300
Query 20000 mention is 21309.168300
The time of 20000 random operations is 21309.168300
Total Result: 21309.168300 21309.168300 21309.168300 21309.168300
21309.168300 21309.168300 21309.168300
5. Conclusion
- Sending a tweet takes the most amount of time among all the operations. Each time
a tweet is sent, the tweet is parsed for both hashtags and mentions which consumes
a considerable amount of time.
- Zipf Subscribe has the second least performance among all operations since it has to
perform 2*N subscribe operations.
- All other operations are dependent on the number of users and run N number of
times.
