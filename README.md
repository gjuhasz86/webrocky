# WebRocky

## Summary 
WebRocky is a web-based client for the Xiaomi vacuums.

## Motivation
One of the reasons I started this project is that the Android based Mi Home and Roborock applications don't seem to work well on my Android phone. I experienced sudden battery drain issues, so I wanted a way to control the vacuum from my desktop PC.
The other reason was to add some features I missed in the app, such as being able to remember certain positions and zones on the map, or saving and restoring preset no-go zone and no-mop zone profiles.

## Build & run

Dependecies: `git`, `java`, `mill`, `md5sum`

```
$ git clone https://github.com/gjuhasz86/webrocky.git
$ cd webrocky
$ mill proj.dist
$ cp secrets.hocon.sample dist/secrets.hocon # SEE EXAMPLE FOR PASSWORD HASH BELOW
$ $EDITOR dist/secrets.hocon # edit the properties
$ cd dist
$ java -cp out.jar roborock.server.WebServer public # Webserver is started at http://localhost:4201
```

Example to generate the md5 hash of your password:
```
$ echo -n "mysecretpassword" | md5sum | tr [:lower:] [:upper:]
4CAB2A2DB6A3C31B01D804DEF28276E6  -
```
(The above example shows the actual output for the password `mysecretpassword`.
If you generate the hash by any other means, make sure it matches this example.) 

## Appendix
### Legal

Xiaomi is a registered trademark and service mark of Xiaomi Inc., which is not 
affiliated with the maker of this program and does not endorse, service or 
warrant the functionality of this product.
