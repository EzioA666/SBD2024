(* Updating Recent Posts

   Notes: An online post (defined by the record type `post` below) has
   a title, content and timestamp.  A user is defined by a record type
   with many fields (see below) but the important fields for this
   problem are

     old_posts: a list of posts in decreasing order of timestamp
     recent_posts: a list of posts in decreasing order of timestamp

   These fields have the further property that every post in
   `old_posts` is older then every post in `recent_posts` (i.e., their
   timestamps are smaller).

   Problem:
   Implement a function `update_recent` which given

     u : a user
     time : a nonnegative integer
     stale : a nonnegative integer

   returns a new `user` with the following properties:

   * every post in `recent_post` at least `stale` timesteps old (with
   respect to the current time `time`) has been moved to `old_posts`

   * the ordering property above is maintained: timestamps still
   appear in decreasing order, and `old_post`s are older than
   `recent_post`s.

   Example:
   let _ = assert (update_recent (mk [] [p 30;p 20;p 10;p 0]) 50 30 = mk [p 20;p 10;p 0] [p 30])
   (* see below for the definition of `mk` and `p` *)

*)

type post = {
  title : string ;
  content : string ;
  timestamp : int ;
}

type user = {
  username : string ;
  email : string ;
  time_joined : int ;
  is_paid_user : bool ;
  balance : int ;
  next_payment_time : int ;
  is_paused : bool ;
  num_followers : int ;
  num_likes : int ;
  old_posts : post list ;
  recent_posts : post list ;
}

let update_recent (u : user) (time : int) (stale : int) : user =
 (* Separate recent_posts into posts to move to old_posts and posts to keep in recent_posts *)
 let rec partition (to_move : post list) (to_keep : post list) (posts : post list) =
  match posts with
  | [] -> (to_move, to_keep)
  | post :: rest ->
      if time - post.timestamp >= stale then
        partition (post :: to_move) to_keep rest
      else
        partition to_move (post :: to_keep) rest
in
let (posts_to_move, remaining_recent_posts) = partition [] [] (List.rev u.recent_posts) in
(* We reversed the list for partitioning to maintain the order of timestamps when adding to to_move and to_keep *)

(* Update the user's posts *)
{
  u with
  old_posts = List.rev_append posts_to_move u.old_posts; (* Merge and reverse to maintain decreasing order *)
  recent_posts = List.rev remaining_recent_posts; (* Reverse to restore original decreasing order *)
}

let p t = {title="";content="";timestamp=t}
let mk op rp = {
  username = "" ;
  email = "" ;
  time_joined = 0 ;
  is_paid_user = true ;
  balance=0 ;
  next_payment_time = 0;
  is_paused = true ;
  num_followers = 0 ;
  num_likes  = 0 ;
  old_posts = op;
  recent_posts = rp;
}
