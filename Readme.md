#VideoCapture

A simple application to capture still images from a webcam or a video acquisition card.


## Environment

### Build

Delphi 10.4 Community Edition (Free [download](https://www.embarcadero.com/products/delphi/starter) courtesy of Embarcadero)

Microsoft DirectX 9 or above.

Tested on:

- Windows XP / Osprey 100 card / Canon RE-350 camera
- Windows 10 / Avermedia H727 card

### Port to Lazarus

I tried to port the project to Lazarus, unfortunately the code uses the AllocateHWND function wich is not available on Lazarus. 

### ini file

At startup the program creates an ini file to save the application settings :

```ini
[PHOTO]
PATH=E:\temp\test.jpg
CAMERA=AVerMedia BDA Analog Capture
```

## Credits


Based on the Delphi 2007 project :

https://www.delphibasics.info/home/delphibasicsprojects/directxdelphiwebcamcaptureexample

Author: Michael Braun (michael@grizzlymotion.com)

License Zlib : http://www.zlib.net/zlib_license.html

Uses DirectX headers from http://www.clootie.ru/delphi/ (see Common\readme.txt)