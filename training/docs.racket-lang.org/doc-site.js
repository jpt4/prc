function AddLogoToMainDiv()
{
 var main_div = document.getElementsByClassName("main")[0];
 var h = document.createElement('div');
 h.setAttribute("class", "docsite-logo");
 h.innerHTML = "<a href=\"https://racket-lang.org/\"><img src=\"https://racket-lang.org/logo-and-text-1-2.png\" alt=\"Racket\" /></a>";
 main_div.insertBefore(h, main_div.firstChild);
 }
AddOnLoad(AddLogoToMainDiv);
