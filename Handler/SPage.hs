module Handler.SPage where

import Import
import Handler.Global

pageForm :: Maybe SPage -> Form SPage
pageForm page = renderDivs $ SPage
    <$> areq textField "Title" (sPageTitle <$> page)
    <*> areq nicHtmlField "Body" (sPageBody <$> page)
    <*> areq textField "Alias" (sPageAlias <$> page)

pageWidget :: Entity SPage -> Widget
pageWidget page = [whamlet|
    <div .page>
        <h2>#{sPageTitle $ entityVal page}
        <div>#{sPageBody $ entityVal page}
|]

pageFormStyles :: Widget
pageFormStyles = toWidget [lucius|
    textarea.html {
        width:530px;
        height:300px;
        }
|]

getCreateSPageR :: Handler RepHtml
getCreateSPageR = do
    ((_, widget), enctype) <- generateFormPost (pageForm Nothing)
    specificLayout (menu CreateSPageR, emptyWidget, [whamlet|
    ^{pageFormStyles}
<form method=post action=@{CreateSPageR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|])

postCreateSPageR :: Handler RepHtml
postCreateSPageR = do
    ((result, widget), enctype) <- runFormPost (pageForm Nothing)
    case result of
        FormSuccess page -> do
            _ <- runDB $ insert page
            redirect $ SPageR $ unpack (sPageAlias page)
        _ -> specificLayout (menu CreateSPageR, emptyWidget, [whamlet|
        Something went wrong. Please submit to the form again.
<form method=post action=@{CreateSPageR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|])

getEditSPageR :: SPageId -> Handler RepHtml
getEditSPageR pageId = do
    page <- runDB $ get404 $ pageId
    ((_, widget), enctype) <- generateFormPost (pageForm $ Just page)
    specificLayout (menu $ EditSPageR pageId, emptyWidget, [whamlet|
    ^{pageFormStyles}
<form method=post action=@{EditSPageR pageId} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|])

postEditSPageR :: SPageId -> Handler RepHtml
postEditSPageR pageId = do
    ((result, widget), enctype) <- runFormPost (pageForm Nothing)
    case result of
        FormSuccess page -> do
            _ <- runDB $ update pageId [SPageTitle =. sPageTitle page,
                                        SPageBody =. sPageBody page,
                                        SPageAlias =. sPageAlias page]
            redirect $ SPageR (unpack $ sPageAlias page)
        _ -> specificLayout (menu $ EditSPageR pageId, emptyWidget, [whamlet|
    Something went wrong. Please try again.
<form method=post action=@{EditSPageR pageId} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|])

getDeleteSPageR :: SPageId -> Handler RepHtml
getDeleteSPageR pageId = do
    page <- runDB $ get404 pageId
    specificLayout (menu $ DeleteSPageR pageId, emptyWidget, [whamlet|
    Are you sure you want to delete this page?
<form method=post action=@{DeleteSPageR pageId}>
    <input type=submit>
<form method=get action=@{SPageR (unpack $ sPageAlias page)}>
    <input type=submit>

|])

postDeleteSPageR :: SPageId -> Handler RepHtml
postDeleteSPageR pageId = do
    _ <- runDB $ delete pageId
    redirect $ AdminR


getSPageR :: String -> Handler RepHtml
getSPageR alias = do
    page <- runDB $ getBy404 $ UniqueAlias (pack alias)
    specificLayout (menu $ SPageR alias, emptyWidget, pageWidget page)

