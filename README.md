# elm-image-labeller-app
Quickly label images for small set of labels

This is a little app that is used to quickly label images for various ML models.
Given a set of urls to images

(In its current form, it has some code that assums that the image is the image of a domain)
[] Convert domain->name

### Building / Running
* Install Elm
* elm make src/Label.elm --output labels.js
* open index.html in browser