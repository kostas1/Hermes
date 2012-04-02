module Handler.Post where
import Import
import Handler.Global

getPostTags :: GHandler Hermes Hermes [Entity Tag]
getPostTags = do
    tags <- runDB $ selectList [] [Desc TagName]
    return tags

getCreatePostR :: Handler RepHtml
getCreatePostR = do
    tags <- getPostTags
    ((_, widget), enctype) <- generateFormPost (postForm (Nothing, tags, Nothing))
    specificLayout (menu CreatePostR, menu CreatePostR, [whamlet|
<form method=post action=@{CreatePostR} enctype=#{enctype}>
    ^{postEditWidgetStyles}
    ^{widget}
    <input type=submit>
|])

insertPost :: Post -> [TagId] -> Maybe Text -> GHandler Hermes Hermes (PostId, Text)
insertPost (Post t c b a p _) selectedTags generatedTags = do
    let slug = (pack (concat $ intersperse "-" $ take 5 $ words (unpack t)))
    postId <- runDB $ insert (Post t c b a p slug)
    assignPostTags postId selectedTags generatedTags
    return (postId, slug)

updatePost :: PostId -> Post -> [TagId] -> Maybe Text -> GHandler Hermes Hermes ()
updatePost postId (Post t c b _ p _) selectedTags generatedTags = do
    assignPostTags postId selectedTags generatedTags
    runDB $ update postId [PostTitle =. t,
                           PostCreated =. c,
                           PostBody =. b,
                           PostPublished =. p]

assignPostTags :: PostId -> [TagId] -> Maybe Text -> GHandler Hermes Hermes ()
assignPostTags postId selectedTags generatedTags = do
    tags <- case generatedTags of
                Nothing -> return selectedTags
                Just t -> do
                    let nt = (splitOn (pack ",") t)
                    inserted <- mapM (\a -> (runDB $ insertBy (Tag a))) nt
                    case (rights inserted) of
                        n@(_:_) -> return $ selectedTags ++ n
                        _ -> return selectedTags

    runDB $ deleteWhere [PostTagPost ==. postId,
                         PostTagTag /<-. tags]

    _ <- mapM (\a -> runDB $ insertBy (PostTag postId a)) tags

    return ()


extractPostTags :: Maybe Text -> GHandler Hermes Hermes (Maybe [Entity Tag])
extractPostTags Nothing = return Nothing
extractPostTags (Just t) = do
    let tags = (splitOn (pack ",") t) :: [Text]
    case tags of
        (_:_) -> do
            results <- mapM (\tag -> (runDB $ insertBy (Tag tag))) tags
            case results of
                (_:_) -> return $ Just $ (lefts results)
                _ -> return Nothing
        _ -> return Nothing



newTagsCorrect :: Maybe Text -> Bool
newTagsCorrect Nothing = True
newTagsCorrect (Just tags) = all (\a -> isLetter a || a == ',') tags

postCreatePostR :: Handler RepHtml
postCreatePostR = do
    tags <- getPostTags
    ((result, widget), enctype) <- runFormPost (postForm (Nothing, tags, Nothing))
    case result of
        FormSuccess ((Post t c b a p s), selectedTags, newTags) -> do
            case newTagsCorrect newTags of
                False -> invalidArgs [pack "Tags textfield contains bad value. Please follow formatting."]
                True -> do
                    (_, slug) <- insertPost (Post t c b a p s) selectedTags newTags
                    redirect $ PostR slug
        _ -> specificLayout (menu CreatePostR, menu CreatePostR, [whamlet|
    Invalid input, try again.
    <form method=post action=@{CreatePostR} enctype=#{enctype}>
        ^{widget}
        <input type=submit>
|])

postEditWidgetStyles :: Widget
postEditWidgetStyles = toWidget [lucius|
    textarea.html {
        width:530px;
        height:300px;
        }
|]

postEditWidget :: PostId -> Widget -> Enctype -> Widget
postEditWidget postId form enctype = [whamlet|
    ^{postEditWidgetStyles}
    <div .post-edit>
        <form method=post action=@{EditPostR postId} enctype=#{enctype}>
            ^{form}
            <input type=submit>
|]


postForm :: (Maybe Post, [Entity Tag], Maybe [TagId]) -> Form (Post, [TagId], Maybe Text)
postForm (post, tags, selected) = do
    renderDivs $ mk
        <$> areq textField "Title" { fsId = Just "title" } (postTitle <$> post)
        <*> aformM (liftIO getCurrentTime)
        <*> areq nicHtmlField "Body" (postBody <$> post)
        <*> aformM requireAuthId
        <*> areq boolField "Published" { fsClass = ["inline-field"]} (postPublished <$> post)
        <*> areq (multiSelectFieldList [(tagName $ entityVal n, entityKey n) | n <- tags]) "Select tags" { fsClass = ["tagselect"] } selected
        <*> aopt textField "Add new tags: words without spaces, separated with comma: " Nothing
            where mk title time body aid published selectedTags newTags = (Post title
                                                                                time
                                                                                body
                                                                                aid
                                                                                published
                                                                                (case post of
                                                                                    Nothing -> ""
                                                                                    Just p -> postSlug p),
                                                                                selectedTags,
                                                                                newTags)

commentForm :: PostId -> Form Comment
commentForm postId = renderDivs $ mk
    <$> areq textField "Author" Nothing
    <*> areq textField "Body" Nothing
    <*> aformM (return postId)
    <*> aformM (liftIO getCurrentTime)
    <*> recaptchaAForm
        where mk title author post time () = Comment title author post time

getPostR :: Text -> Handler RepHtml
getPostR slug = do
    authorized <- maybeAdmin
    (Entity postId post) <- runDB $ getBy404 $ UniqueSlug slug
    postTags <- runDB $ selectList [PostTagPost ==. postId] []
    tags <- runDB $ selectList [TagId <-. map (\e -> postTagTag $ entityVal e) postTags] []
    case (postPublished post) of
        False -> notFound
        True -> do
            comments <- runDB $ selectList [CommentPost ==. postId] [Desc CommentCreated]
            ((_, widget), enctype) <- generateFormPost (commentForm postId)

            specificLayout (menu $ PostR $ postSlug post, emptyWidget, [whamlet|
            $if authorized
                <div .controls>
                    <ul>
                        <li>
                            <a href=@{EditPostR postId}>Edit
                        <li>
                            <a href=@{DeletePostR postId}>Delete
            <div .single-Post>
                <div .header>
                    <h2>
                        #{postTitle post}
                    <span .meta>
                        #{show $ postCreated post}
                <div .body>
                    #{postBody post}
                <div .tags>
                    <ul>
                        $forall tag <- tags
                            <li>
                                <a href=@{TagR (tagName $ entityVal tag)}>#{tagName $ entityVal tag}
            <div .comments>
                <h2>
                    Comments
                $forall comment <- comments
                    <div .pretty.comment>
                        <span .header>
                            Created on #{show $ commentCreated (entityVal comment)} by #{commentAuthor (entityVal comment)}
                        <div .body>
                            #{commentBody (entityVal comment)}

                <h2>
                    Create new comment
                <div .pretty.comment-form>
                    <form method=post action=@{PostR $ postSlug post} enctype=#{enctype}>
                        ^{widget}
                        <input type=submit>
|])

postPostR :: Text -> Handler RepHtml
postPostR slug = do
    (Entity postId _) <- runDB $ getBy404 $ UniqueSlug slug
    ((result, widget), enctype) <- runFormPost (commentForm postId)
    case result of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessage "Thanks for comment!"
            redirect $ PostR slug
        _ -> specificLayout (menu $ PostR slug, emptyWidget, [whamlet|
    Something went wrong. Please check your comment again.
    <form method=post action=@{PostR slug} enctype=#{enctype}>
        ^{widget}
        <input type=submit>
|])

getDeletePostR :: PostId -> Handler RepHtml
getDeletePostR postId = do
    _ <- runDB $ get404 postId
    specificLayout (menu $ DeletePostR postId, menu $ DeletePostR postId, [whamlet|
    Are you sure you want to delete this post?
    <form method=post action=@{DeletePostR postId}>
        <input type=submit value=Yes>
    <form method=get action=@{AdminR}>
        <input type=submit value=No>

|])

postDeletePostR :: PostId -> Handler RepHtml
postDeletePostR postId = do
    runDB $ deleteWhere [CommentPost ==. postId]
    runDB $ deleteWhere [PostTagPost ==. postId]
    runDB $ delete postId
    setMessage "Post has been deleted."
    redirect AdminR

getEditPostR :: PostId -> Handler RepHtml
getEditPostR postId = do
    tags <- getPostTags
    post <- runDB $ get404 postId
    selected <- (runDB $ selectList [PostTagPost ==. postId] [])

    ((_, widget), enctype) <- generateFormPost (postForm (Just post, tags, Just $ map (\e -> postTagTag $ entityVal e) selected))
    specificLayout (menu $ EditPostR postId, menu $ EditPostR postId, [whamlet|
        ^{postEditWidget postId widget enctype}
|])

postEditPostR :: PostId -> Handler RepHtml
postEditPostR postId = do
    tags <- getPostTags
    p <- runDB $ get404 postId
    ((result, widget), enctype) <- runFormPost (postForm (Just p, tags, Nothing))
    case result of
        FormSuccess (post, selectedTags, newTags) -> do
            case newTagsCorrect newTags of
                False -> invalidArgs [pack "Tags textfield contains bad value. Please follow formatting."]
                True -> do
                    updatePost postId post selectedTags newTags
                    setMessage "Post has been updated."
                    redirect $ PostR $ postSlug post
        _ -> specificLayout (menu $ EditPostR postId, menu $ EditPostR postId, [whamlet|
    Something went wrong. Check the post again:
<form method=post action=@{EditPostR postId} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|])

getPostsR :: Handler RepHtml
getPostsR = do
    posts <- runDB $ selectList [PostPublished ==. True] [Desc PostCreated]
    tags <- tagsWidget
    specificLayout (menu PostsR, tags, [whamlet|
    <div .posts>
        $forall e <- posts
            <div .post ##{show $ entityKey e}>
                <div .header>
                    <h2>
                        <a href=@{PostR $ postSlug $ entityVal e}>
                            #{postTitle (entityVal e)}
                    <span .meta>
                        Posted on #{show $ postCreated (entityVal e)}
                <div .body>
                    #{postBody (entityVal e)}
            <hr .rule>
|])

postListingWidget :: Entity Post -> Widget
postListingWidget e = [whamlet|
    <div .post ##{show $ entityKey e}>
                <div .header>
                    <h2>
                        <a href=@{PostR $ postSlug $ entityVal e}>
                            #{postTitle (entityVal e)}
                    <span .meta>
                        Posted on #{show $ postCreated (entityVal e)}
                <div .body>
                    #{postBody (entityVal e)}
            <hr .rule>
|]

getTagR :: Text -> Handler RepHtml
getTagR tag = do
    tagId <- runDB $ selectFirst [TagName ==. tag] []
    case tagId of
        Nothing -> notFound
        Just i -> do
            postTags <- runDB $ selectList [PostTagTag ==. entityKey i] []
            let postIds = map (\e -> postTagPost $ entityVal e) postTags
            posts <- runDB $ selectList [PostId <-. postIds] []
            specificLayout (menu $ TagR tag, emptyWidget, [whamlet|
    $forall post <- posts
        ^{postListingWidget post}
|])

tagsWidget :: GHandler Hermes Hermes Widget
tagsWidget = do
    tags <- runDB $ selectList ([] :: [Filter Tag]) []
    results <- mapM (\t -> do
        c <- runDB $ count [PostTagTag ==. (entityKey t)]
        return (t, c)) tags
    return (tagsWidgetLayout results)

tagsWidgetLayout :: [(Entity Tag, Int)] -> Widget
tagsWidgetLayout results = do
    [whamlet|
    <div .tags>
        <ul>
            $forall result <- results
                <li>
                    <a href=@{TagR (tagName $ entityVal $ fst result)}>
                        #{tagName $ entityVal $ fst result} (#{snd result})

|]
