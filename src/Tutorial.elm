module Tutorial exposing (..)

import Html
import HtmlExtra


script =
    HtmlExtra.script [] [ Html.text """
function startIntro(){
    var intro = introJs();
    intro.setOptions({
        steps: [
            {
                intro: 'Welcome to Yummy Goodness.'
            },
            {
                element: '.wedge',
                intro: 'Buy things.',
                position: 'bottom'
            },
            {
                element: '.menus div',
                intro: 'Drop things.',
                position: 'bottom'
            },
            {
                element: '.ground',
                intro: 'Pick things up.',
                position: 'top'
            },
            {
                element: '.player:not(.selected)',
                intro: 'Select players.',
                position: 'bottom'
            },
            {
                element: '.next',
                intro: 'Start rounds.',
                position: 'bottom'
            },
            {
                element: '.summary',
                intro: 'Win or lose after 15 rounds.',
                position: 'bottom'
            }
        ]
    });
    intro.start();
}
""" ]


startOnClick =
    HtmlExtra.onclick "startIntro();"


startOnLoad =
    HtmlExtra.onload "startIntro();"
