{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Global where

import Import

menu :: Route Hermes -> Widget
menu current = [whamlet|
    <ul>
        <li>
            ^{createLink PostsR current "Posts"}
        <li>
            ^{createLink (SPageR "idea") current "Idea"}
        <li>
            ^{createLink (SPageR "roadmap") current "Roadmap"}
|]


createLink :: Route Hermes -> Route Hermes -> String -> Widget
createLink specific current label = [whamlet|
    $if current == specific
        <a .active href=@{specific}>#{label}
    $else
        <a href=@{specific}>#{label}
|]

emptyWidget :: Widget
emptyWidget = toWidget [hamlet|nu|]

frontIncludes :: Widget
frontIncludes = do
--  addStylesheet (StaticR style_css)
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.18/jquery-ui.min.js"
    addScript (StaticR js_sh_main_min_js)
    addStylesheet (StaticR css_sh_bright_min_css)

frontWidget :: (Widget, Widget, Widget) -> Widget
frontWidget (menuWidget, sidebarWidget, contentWidget) = [whamlet|
    ^{frontIncludes}
    ^{recaptchaOptions (RecaptchaOptions (Just "clean") Nothing)}
    <div id="header">
        <div id="logo">
            <a href="/">
                <span class="orange">Hermes
        <div id="menu">
            ^{menuWidget}
    <div id="main">
        <div id="content">
            <div id="text">
                ^{contentWidget}

            <div id="sidebar">
                ^{sidebarWidget}
    <div id="footer">
        <div id="left_footer">

        <div id="right_footer">
            --respect for the design creators! o/
            <a href="http://www.realitysoftware.ca/services/website-development/design/">Web design</a> released by <a href="http://www.flash-gallery.org/">Flash Gallery</a

|]

specificLayout :: (Widget, Widget, Widget) -> GHandler Hermes Hermes RepHtml
specificLayout (a, b, c) = defaultLayout $ frontWidget (a, b, c)


getAdminR :: Handler RepHtml
getAdminR = do
    posts <- runDB $ selectList [] [Desc PostCreated]
    pages <- runDB $ selectList [] [Desc SPageTitle]
    comments <- runDB $ selectList [] [Desc CommentCreated]
    tags <- runDB $ selectList ([] :: [Filter Tag]) []
    specificLayout (menu AdminR, emptyWidget, [whamlet|
    <div>
        <h2>Add content
        <ul>
            <li>
                <a href=@{CreatePostR}>Create Post
            <li>
                <a href=@{CreateSPageR}>Create page
        <h2>Change content
        <div .admin-content-view>
            <div>
                <h2>Posts
                $forall post <- posts
                    <div>
                        <a href=@{EditPostR $ entityKey post}>Edit
                        <a href=@{DeletePostR $ entityKey post}>Delete
                        #{postTitle (entityVal post)}
            <div>
                <h2>Pages
                $forall page <- pages
                    <div>
                        <a href=@{EditSPageR $ entityKey page}>Edit
                        <a href=@{DeleteSPageR $ entityKey page}>Delete
                        #{sPageTitle (entityVal page)}
            <div>
                <h2>Comments
                $forall comment <- comments
                    <div>
                        <a href=@{DeleteCommentR $ entityKey comment}>Delete
                        #{commentAuthor (entityVal comment)}
                        #{commentBody (entityVal comment)}
            <div>
                <h2>Tags
                $forall tag <- tags
                    <div>
                        <a href=@{DeleteTagR $ entityKey tag}>Delete
                        #{tagName (entityVal tag)}


|])

test :: forall a. (Integral a) => a -> Int
test _ = 1

getDeleteCommentR :: CommentId -> Handler RepHtml
getDeleteCommentR commentId = do
    _ <- runDB $ delete commentId
    redirect $ AdminR

getDeleteTagR :: TagId -> Handler RepHtml
getDeleteTagR tagId = do
    let aasd = approot :: Approot Hermes
    y <- getYesod
    case aasd of
        ApprootStatic t -> setMessage $ toHtml t
        ApprootMaster f -> setMessage $ toHtml $ f y
        _ -> setMessage "a"
    runDB $ deleteWhere [PostTagTag ==. tagId]
    _ <- runDB $ delete tagId
    redirect $ AdminR

getRssR :: Handler RepRss
getRssR = do
    t <- liftIO getCurrentTime
    posts <- runDB $ selectList [] [Desc PostCreated]
    let entries = map (\p -> (FeedEntry (PostR $ postSlug $ entityVal p) (postCreated $ entityVal p) (postTitle $ entityVal p) (postBody $ entityVal p))) posts
    rssFeed $ Feed "Hermes" RssR PostsR (toHtml ("An attempt to create a simple blog in Yesod" ::String)) "en" t entries
