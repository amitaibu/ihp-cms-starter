module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = [hsx|
        <div class="container-wide">
            <ol class="list-decimal">
                <li><a href={ArticlesAction}>Articles</a></li>
                <li><a href={LandingPagesAction}>Landing pages</a></li>
                <li><a href={ParagraphCtaAction}>Paragraph CTAs</a></li>
                <li><a href={ParagraphQuotesAction}>Paragraph quotes</a></li>
            </ol>
         </div>
|]