<!-- in esqeuleto.experimental is it
lowercase or uppercase t in table?  -->

1) how about innerJoin / leftJoin / union / table  is the first letter capitalized?


2) ok so leftJoin just flat out isnt defined below?

select $ do
(people :& blogPosts) <-
    from $ table @Person
    `leftJoin` table @BlogPost
    `on` (\(people :& blogPosts) ->
            people ^. PersonId ==. blogPosts ?. BlogPostAuthorId)
where_ (people ^. PersonAge >. val 18)
pure (people, blogPosts)