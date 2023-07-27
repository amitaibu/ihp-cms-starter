module Web.Mail.LandingPages.LandingPageView where
import Web.View.Prelude
import IHP.MailPrelude

data LandingPageViewMail = LandingPageViewMail { landingPage :: LandingPage }

instance BuildMail LandingPageViewMail where
    subject = "Subject"
    to LandingPageViewMail { .. } = Address { addressName = Just "Firstname Lastname", addressEmail = "fname.lname@example.com" }
    from = "hi@example.com"
    html LandingPageViewMail { .. } = [hsx|
        Hello World
    |]
