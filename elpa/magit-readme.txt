<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
<head><base href="https://n118.network-auth.com/splash/" />
  <title>Free Downtown Internet - DWBIA</title>

  <style type="text/css" media="screen">
    /* body: #f7f7f7 #676767 #1682B9
   c1:   #fff    #858585 #FFFFFF
   c2:   #f7f7f7 #DDDDDD #66bc29
*/

html { font-size: 100%; -webkit-text-size-adjust: 100%; -ms-text-size-adjust: 100%; }
form { margin: 0; }
label { cursor: pointer; }
button, input { font-size: 100%; margin: 0; vertical-align: baseline; *vertical-align: middle; line-height: normal; *overflow: visible; }
button::-moz-focus-inner, input::-moz-focus-inner { border: 0; padding: 0; }

a, a:visited, a:active {color: #1682b9; text-decoration: none;}
a:hover {color: #444;}

.hidden { display: none; }

body {
  margin: 0;
  background: #f7f7f7;
  color: #676767;
  font-size: 14px;
  line-height: 1.4em;
  font-family: georgia, palatino, serif;
}

h1, h2 { font-family: helvetica, helvetica neue, verdana, tahoma, sans-serif; }
h1 {
  font-size: 24px;
  font-weight: bold;
  color: #676767;
  margin: 0;
}
h2 {
  font-size: 14px;
  font-weight: normal;
}

.header {
  border-bottom: 1px solid #d1d1d1;
  padding: 0 0 .5em;
  margin: 0 0 1em 0;
}

.header table { width: 100%; }
#title_cell { vertical-align: bottom; }
#icon_cell { text-align: right; }

#container {
  max-width: 900px;
  min-width: 300px;
  margin: 0 auto;
  border-top: 12px solid #66bc29;
  padding: 0.5em 30px;
  background: #ffffff;
  box-shadow:0px 0px 10px #BFBFBF
}

#left_col {
  float: left;
  width: 100%;
}

#right_col {
  float: left;
  width: 420px;
  margin-left: -420px;
}

#left_col_inner {
  margin-right: 420px;
  padding-right: 30px;
}

#blocked_msg {
  text-align: center;
  padding: 15px 0;
}

#claim_ap_msg {
  text-align: center;
  padding: 15px 0;
}

.formarea {
  background: #f7f7f7;
  color: #5e5e5e;
  padding: 15px;
  border: 1px solid #cfcfcf;
  -moz-border-radius: 3px;
  border-radius: 3px;
  margin-bottom: 20px;
}

.formarea ul {
  list-style: none;
  padding: 0;
  margin: 0;
}

.formarea li { margin-top: 22px; }
.formarea li:first-child { margin-top: 10px; }
.formarea br { display: none; }

.formarea label {min-height: 30px; margin: 1em 0 14px; padding: 0 0 15px;}
.formarea input {width: 95%;}
/*.formarea input:hover, .formarea input:focus {border-color: #aaa; box-shadow: none;}*/
.formarea input, .formarea textarea {
  padding: 7px;
  background: #ffffff;
  color: #676767;
  border: solid 1px #ccc;
  font-size: 1em;
}

.formarea select, .formarea textarea {font-size: 1em;}
.formarea .color_button { width: 50%; }

.color_button {
  padding: 0;
  -moz-border-radius: 3px;
  border-radius: 3px;
  font-style: normal;
  font-weight: 600;
  background: #66bc29;
  white-space: nowrap;
  display: inline-block;
  background: -moz-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(98, 158, 25, 1) 100%);
  background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba(102, 188, 41, 1)), color-stop(100%,rgba(98, 158, 25, 1)));
  background: -webkit-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(98, 158, 25, 1) 100%);
  background: -o-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(98, 158, 25, 1) 100%);
  background: -ms-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(98, 158, 25, 1) 100%);
  filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#66bc29', endColorstr='#639e1a',GradientType=0 );
  background: linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(98, 158, 25, 1) 100%);
}

.color_button:hover, .color_button:focus {
  background: #5f800a;
  background: -moz-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(95, 128, 10, 1) 100%);
  background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba(102, 188, 41, 1)), color-stop(100%,rgba(95, 128, 10, 1)));
  background: -webkit-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(95, 128, 10, 1) 100%);
  background: -o-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(95, 128, 10, 1) 100%);
  background: -ms-linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(95, 128, 10, 1) 100%);
  filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#66bc29', endColorstr='#5f800a',GradientType=0 );
  background: linear-gradient(top, rgba(102, 188, 41, 1) 0%, rgba(95, 128, 10, 1) 100%);
}

.color_button.wider { padding: 12px 30px; }

.color_button input, .color_button a {
  padding: 12px 3px;
  display: block;
  border: none;
  background: none;
  width: 100%;
  height: auto;
  color: #ffffff;
  -webkit-appearance: none;
  -moz-appearance: none;
  margin: -3px auto;
  cursor: pointer;
}

.form_links {
  float: right;
  margin-top: -2em;
  width: 45%;
  text-align: center;
}

.footer {
  clear: both;
  border-top: 1px solid #d1d1d1;
  padding: 0.5em 0 0 3px;
  margin: 1em 0 0 0;
  color: #d1d1d1;
}

.footer img {
  margin-left: 5px;
  vertical-align: top;
}

.error_outer {
  margin: 0 auto 1em;
  background: #ea4a52;
  background: rgba(234, 74, 82, 0.5);
  padding: 4px;
  border-radius: 3px;
}
.error_inner {
  background: #ea4a52;
  color: #fff;
  padding: 0.3em 20px;
  text-align: center;
}
.error_inner p {font-family: Helvetica, Verdana, arial, sans-serif;}
.error_inner p span {font-size: 1.5em; font-weight: bold; margin-right: .8em;}

input:invalid, textarea:invalid { background-color: #f0dddd; }
.formarea input:invalid, textarea:invalid { border-radius: 1px; -moz-box-shadow: 0px 0px 0px red; -webkit-box-shadow: 0px 0px 0px red; box-shadow: 0px 0px 0px red; }
.formarea label.invalid {border: none; color: red; min-height: inherit; padding: 5px 0 0; margin: 0;}

@media only screen and (max-width: 730px) {
  #left_col_inner { margin: 0; padding: 0; }
  #right_col {
    float: none;
    clear: both;
    margin: 0 auto;
  }
}

@media only screen and (max-width: 480px) {
  #container { padding: 0.5em 5px; }
  #right_col { width: 100%; }
}


    #container { max-width: 700px; }
  </style>
  <meta name="viewport" content="width=device-width,initial-scale=1" >
</head>

<body>
  <div id="container">

    <div class="header">
      <table><tr>
        <td id="title_cell"><h1>Free Downtown Internet - DWBIA</h1></td>
        <td id="icon_cell"><img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAALsAAABWCAYAAACXSuc2AAAABmJLR0QA/wD/
AP+gvaeTAAAACXBIWXMAAAsRAAALEQF/ZF+RAAAc/klEQVR42u2dB3yO1xfH
0VKzqrVLVVsttamataqqSs3qMKva2hSxR4wY/VsRQkRitEYSJIhNRCRGYsSW
gSADRZbscP73d973efO8K29IUuuez+d+kvd5n/U+z/fee+6555ybh6RIeUUk
j3wEUiTsUqRI2KVIkbBLkSJhlyJFwi5FioRdihQJuxQpEnYpUiTsUqRI2KVI
2KVIkbDnnjx6lEppaYmUnpaU6+Xx4/RM72X7tm10MiBAkiFhzx1JTLhDMdEh
ooRq/+ZG0Zw7NfWh0fXDrofp/u/RrTvZLV4syZCw5xbsdxnEhIdRlJR0j5IS
/83ZIs4ZH3fLCPbHjx/TIS8vatqoMZ0/d07sl0QD+venv9eupTt37lBQUJDe
fWLbiePHKTUtTZIjYc9Gy/4gWKgZibl3DQF9tLiGGvbU1FQa9McfVKXyB9S/
3y90/fp1sho1mrp37UbjrMZSk4YN6a9583hfzx07uNVHZfjh+x5069YtSY+E
/elhT0tNyEXY7xrBDgkJDqbWLVpSSnIyf+7ftx/91ONHMYZIo/Pnz1P92nXo
woUL9FXrL+ls4FneZ8jgwTRx/HhJj4Q9+7BDTVh9+AjZ7/cih4PeT1Vs9+wn
/9CrFmEPunKFWjVvQclChYH80qcvrXF21g6cH1GHdt/QOqHWNPrsMxo5fDgN
GTSYunbuTPPmzJX0SNizD3uCaGU7L7SjVjbzqO3c+U9VGk+zoTWHDluE/aJo
tZs3bab7DNhXrljB/0OH/1bAvtnNjZoJvf7ff/+VxEjYc2CAqoI9MSWF+ix3
FMAvoe62y56qtJu3kDb6HrUI+53bt+mzOnVp7OgxdP/+ffr5hx/JdtEi7TGJ
rMZgoDph3HjW5Tf8s54WL1xIV1W9hpQXGHbYo5OTHgg9NuY/KQ/jw9lS8ixg
hxw/doymTp5CDx48oCM+PnRB6Or8HNLTyX3LVoqLi+PPmzZupBnW1rTUzk62
8i8L7MmJ9wUYQQyHpgTlYglm0KMfAPaHzwR2Ka8y7KJVBxgJCbcpWbS8ycnR
/0nBTKqEXcozgD1IgBH3bHT4Zwg7JppQnlag8sTGxmbrHM+7PHz4kH9jSkqq
2Wf4wsGekhL7TG4qOTWdets7UqcFttRt8VJtyR3YMSgd+PvvYvDZla0ymEhq
0rAR29Vhc18vBqTYJzO5LQa5NrNm8fHVqnxMn3xUhWdlp0yaTDdv3tTbF5NR
3bt0pTGjRpk81+5du8S1W9PsWTYmv1+zejW1btWavLy8+DMmwdp91ZZWOzmb
vb/pU6dR106d+XdgPAJTa8f234rSnv/+1OMHHph36tCBt3X4pj192bIl7dmz
R3eOFNEArXZ2pm/btaPqn1SjTz78iAf2v/86gI76HdUf84mxzqiRI/l34tkY
yo2wMJ6g+63/rxQbY8zYiRMnqO2XbcjOdgl/3rF9O//GCePGmf2NR3yOiH2+
Iu9Dh7IP+70HoXQr6hRF3A7M1RJ1J5CuR5ymyW6baPSGf8hq43oa57KBhq9b
S10F9DkN+/Vr16hYocJUueJ71KdXLxo2eAgNGTiIAfj4gw8pb5489OnHn5D7
1q0mH6DXwYP0sXjxr+fJy+D8OWIEjRoxkr5u85XYlkectxJtVR2bkJAgYKlK
ZUuWMqoIEMzo5hHHVRPXjImJMfq+jaiEhQu8QaGhofy5qaiY+cW1K5QtRxcv
XjB5j5grKJS/AN28cYMuX7okQG7FYHfu+B19J+65XKnSVPrtdxiWLt+Jbd92
oDaiQimww02i/dft+L5ggsV8A37jjz160JuFi9BbRYrRHJvZetfs26s37+/h
7m50Pxs3bODnWjj/G+Rz+LDR99aicuLYv9eu4892S+z4+RbI9xr9s26dyd/o
6uLKx2xYvz77sN+M9KdLIZ505eqeXC3B1/bQ+eDdNMvDjaZsdqVpW1xpursb
jRXQ5wbsaGXeKf4W/fh9D6PngK56laMjVSxXjooWLESb3TbrfX/27Fkq805J
erdMWdq5c6eJl7qRzw2Qjh8/rts+cthwyidezF5Vy6nY9dGrVHq3Ar1bugwF
+PvrfR92I4zKlypD3wlQFWkr4Mf1SxQtJkDtZFKF6NG1G9/nlcuXTUJQr1Zt
qip6o7t375pUy9DKouLCdQKttqElq9anNRje5fb2Gb99/QaGb9rUqUbnRE+K
Z1Ki2Ju61lutAn3Tti0/g8iISN7msGIF3/8H71XiHiU8PNzonO5b3UXlKUBu
rq7Zhz1ctOoM4/UDuVpCww7QxdD9NMPdlSa5uQjgXWjaVlcaI1r5/xp2RdA1
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
gg==
' /></td>
      </tr></table>
    </div>

    <div>
      <p>This free service is provided to you by the Downtown Windsor Business Improvement Association and its over 650 member businesses.<br>
<p>
We thank you for visiting downtown Windsor!</p>
      <div class="color_button wider">
        <a href="https://n118.network-auth.com/Free-Downtown-In/hi/v_Cdjdh/grant?continue_url=http://www.downtownwindsorcoupons.ca" onclick = "javascript:(function() {
       var _request = new XMLHttpRequest();
       var url = 'https://n118.network-auth.com/Free-Downtown-In/hi/v_Cdjdh/grant?continue_url=CONTINUE_URL_PLACEHOLDER';
       _request.open('HEAD', window.location, true);
       _request.setRequestHeader('X-Requested-With', 'XMLHttpRequest');
       _request.onreadystatechange = function() {
         if (_request.readyState === 4) {
           var continue_url = _request.getResponseHeader('Continue-Url');
           window.location.href = url.replace('CONTINUE_URL_PLACEHOLDER', continue_url);
         };
       };
       _request.send(null);
     })(); return false;" title="Continue to the Internet" id="continue_link">Continue to the Internet</a>
      </div>
    </div>

    <div class="footer">
      <img alt="Powered by Cisco Meraki" src="/images/powered-by-cisco-meraki-30x220.png" />
    </div>
  </div>

</body>
</html>
