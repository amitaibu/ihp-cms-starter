module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes
import Application.Helper.View
import Web.Element.ElementWrap
import Web.Element.Types

defaultLayout :: UTCTime -> Html -> Html
defaultLayout currentTime inner = [hsx|
<!DOCTYPE html>
<html lang="en">
    <head>
        {metaTags}

        {stylesheets}
        {scripts}

        <title>{pageTitleOrDefault "App"}</title>
    </head>
    <body>
        <div class="flex flex-col min-h-screen">
            <div class="flex-1">
                <div class="font-body text-black flex flex-col gap-y-8 md:gap-y-10">
                    {renderFlashMessages |> wrapContainerWide}
                    {inner}
                </div>
            </div>
            {footer currentTime}
        </div>
    </body>
</html>
|]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <!-- jsDelivr :: Sortable :: Latest (https://www.jsdelivr.com/package/npm/sortablejs) -->
        <script src="https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js"></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
    {autoRefreshMeta}
|]

footer :: UTCTime -> Html
footer currentTime =
  [hsx|
    {icons}
    © {currentYear} Company, All rights reserved.
|]
    |> wrapVerticalSpacing AlignCenter
    |> wrapBackgroundColor Gray300 BackgroundColorPaddingSection
    |> \element -> [hsx|<div class="font-display mt-10">{element}</div>|]
  where
    icons =
      [hsx|
            {- Facebook -}
            <a href="">
                <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path fill-rule="evenodd" clip-rule="evenodd" d="M20 10C20 4.477 15.523 0 10 0C4.477 0 0 4.477 0 10C0 14.991 3.657 19.128 8.438 19.878V12.891H5.898V10H8.438V7.797C8.438 5.291 9.93 3.907 12.215 3.907C13.309 3.907 14.453 4.102 14.453 4.102V6.562H13.193C11.95 6.562 11.563 7.333 11.563 8.124V10H14.336L13.893 12.89H11.563V19.878C16.343 19.128 20 14.991 20 10Z" fill="#475569"/>
                </svg>
            </a>

            {- Instagram -}
            <a href="">
                <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path fill-rule="evenodd" clip-rule="evenodd" d="M12.315 2C14.745 2 15.099 2.013 16.123 2.06C17.187 2.109 17.914 2.278 18.55 2.525C19.2175 2.77587 19.8222 3.16936 20.322 3.678C20.8306 4.17777 21.2241 4.7825 21.475 5.45C21.722 6.086 21.891 6.813 21.94 7.877C21.988 8.944 22 9.284 22 12V12.08C22 14.723 21.988 15.067 21.94 16.123C21.891 17.187 21.722 17.914 21.475 18.55C21.2241 19.2175 20.8306 19.8222 20.322 20.322C19.8222 20.8306 19.2175 21.2241 18.55 21.475C17.914 21.722 17.187 21.891 16.123 21.94C15.056 21.988 14.716 22 12 22H11.92C9.277 22 8.933 21.988 7.877 21.94C6.813 21.891 6.086 21.722 5.45 21.475C4.7825 21.2241 4.17777 20.8306 3.678 20.322C3.16936 19.8222 2.77587 19.2175 2.525 18.55C2.278 17.914 2.109 17.187 2.06 16.123C2.013 15.099 2 14.744 2 12.315V11.685C2 9.255 2.013 8.901 2.06 7.877C2.109 6.813 2.278 6.086 2.525 5.45C2.77587 4.7825 3.16936 4.17777 3.678 3.678C4.17777 3.16936 4.7825 2.77587 5.45 2.525C6.086 2.278 6.813 2.109 7.877 2.06C8.901 2.013 9.256 2 11.685 2H12.315ZM12.234 3.802H11.766C9.31 3.802 8.982 3.813 7.959 3.86C6.984 3.905 6.455 4.067 6.102 4.204C5.635 4.386 5.302 4.602 4.952 4.952C4.602 5.302 4.386 5.635 4.204 6.102C4.067 6.455 3.904 6.984 3.86 7.959C3.813 8.982 3.802 9.31 3.802 11.766V12.234C3.802 14.69 3.813 15.018 3.86 16.041C3.905 17.016 4.067 17.545 4.204 17.898C4.386 18.364 4.603 18.698 4.952 19.048C5.302 19.398 5.635 19.614 6.102 19.796C6.455 19.933 6.984 20.096 7.959 20.14C9.013 20.188 9.329 20.198 12 20.198H12.08C14.677 20.198 14.997 20.188 16.04 20.14C17.016 20.095 17.545 19.933 17.898 19.796C18.364 19.614 18.698 19.398 19.048 19.048C19.398 18.698 19.614 18.365 19.796 17.898C19.933 17.545 20.096 17.016 20.14 16.041C20.188 14.986 20.198 14.671 20.198 12V11.92C20.198 9.323 20.188 9.003 20.14 7.96C20.095 6.984 19.933 6.455 19.796 6.102C19.6358 5.66757 19.3802 5.2746 19.048 4.952C18.7254 4.61986 18.3324 4.36426 17.898 4.204C17.545 4.067 17.016 3.904 16.041 3.86C15.018 3.813 14.69 3.802 12.234 3.802ZM12 6.865C12.6743 6.865 13.3421 6.99782 13.9651 7.25588C14.5881 7.51394 15.1542 7.89218 15.631 8.36901C16.1078 8.84584 16.4861 9.41191 16.7441 10.0349C17.0022 10.6579 17.135 11.3257 17.135 12C17.135 12.6743 17.0022 13.3421 16.7441 13.9651C16.4861 14.5881 16.1078 15.1542 15.631 15.631C15.1542 16.1078 14.5881 16.4861 13.9651 16.7441C13.3421 17.0022 12.6743 17.135 12 17.135C10.6381 17.135 9.33201 16.594 8.36901 15.631C7.40601 14.668 6.865 13.3619 6.865 12C6.865 10.6381 7.40601 9.33201 8.36901 8.36901C9.33201 7.40601 10.6381 6.865 12 6.865ZM12 8.667C11.116 8.667 10.2683 9.01815 9.64321 9.64321C9.01815 10.2683 8.667 11.116 8.667 12C8.667 12.884 9.01815 13.7317 9.64321 14.3568C10.2683 14.9818 11.116 15.333 12 15.333C12.884 15.333 13.7317 14.9818 14.3568 14.3568C14.9818 13.7317 15.333 12.884 15.333 12C15.333 11.116 14.9818 10.2683 14.3568 9.64321C13.7317 9.01815 12.884 8.667 12 8.667ZM17.338 5.462C17.6563 5.462 17.9615 5.58843 18.1865 5.81347C18.4116 6.03852 18.538 6.34374 18.538 6.662C18.538 6.98026 18.4116 7.28548 18.1865 7.51053C17.9615 7.73557 17.6563 7.862 17.338 7.862C17.0197 7.862 16.7145 7.73557 16.4895 7.51053C16.2644 7.28548 16.138 6.98026 16.138 6.662C16.138 6.34374 16.2644 6.03852 16.4895 5.81347C16.7145 5.58843 17.0197 5.462 17.338 5.462Z" fill="#475569"/>
                </svg>
            </a>
        |]
        |> wrapHorizontalSpacing AlignCenter

    currentYear = formatTime defaultTimeLocale "%Y" currentTime