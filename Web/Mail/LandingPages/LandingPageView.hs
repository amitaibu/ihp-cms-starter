module Web.Mail.LandingPages.LandingPageView where
import Web.View.Prelude
import IHP.MailPrelude

data LandingPageViewMail = LandingPageViewMail { landingPage :: LandingPage }

instance BuildMail LandingPageViewMail where
    subject = "Landing page " <> landingPage.title
        where landingPage = ?mail.landingPage
    to LandingPageViewMail { .. } = Address { addressName = Just "Firstname Lastname", addressEmail = "fname.lname@example.com" }
    from = "hi@example.com"
    html LandingPageViewMail { .. } = [hsx|
        Hello World, landing page was just viewed
        {landingPage.title}
    |]
