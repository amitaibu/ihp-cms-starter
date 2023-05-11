module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = [hsx|
        <ol class="list-decimal">
            <li><a href={ArticlesAction}>Articles</a></li>
            <li><a href={ParagraphQuotesAction}>Paragraph quotes</a></li>
            <li><a href={ParagraphFeaturedArticlesAction}>Paragraph featured articles</a></li>
         </ol>
|]