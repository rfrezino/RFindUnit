# RFindUnit - Version 1.2.0

Find unit (find uses) is a very simple tool and very used (Ctrl+Shift+A), but, as you know Delphi Find Unit doesn't work very well, in general it crashes or it's very slow.

Currently it have the basics expected features, and I'm working to organize, optmize and give it new functions (as auto import for example).
If you help, it'll be very good.

### How does it work in practice ?
[![IMAGE ALT TEXT](https://i.ytimg.com/vi/SYNUQcg_y58/hqdefault.jpg)](https://www.youtube.com/watch?v=3Y1GengunuAE "Demonstration")


### [Follow our updates on the blog](https://rfrezinos.wordpress.com/)

### Automatically Identify unused imports
First enable it: 

![Enable feature](https://github.com/rfrezino/RFindUnit/blob/master/Resources/ExperimentalFeature.png)

Process: 

![Process the file](https://github.com/rfrezino/RFindUnit/blob/master/Resources/ProcessingUses.png)

Show if everything is right:

![Show all ok](https://github.com/rfrezino/RFindUnit/blob/master/Resources/CheckedAndOK.png)

Or if there are some unused units:

![Highlight unused uses](https://github.com/rfrezino/RFindUnit/blob/master/Resources/CheckedAndNotOk.png)

### Find unit feature
Before: 

![Default Version](http://i.imgur.com/8DZPGSs.png)

After:

![RFindUnit Screen](https://github.com/rfrezino/RFindUnit/blob/master/Resources/RFindUnitImage.png)

### Auto organize uses feature
It's possible to auto organize uses (Ctrl + Shift + U). It's possible to sort then by alphabetical order and split it by line and namespae, as you can see: 

Before:

![Before](https://github.com/rfrezino/RFindUnit/blob/master/Resources/organizeBefore.png)

After:

![After](https://github.com/rfrezino/RFindUnit/blob/master/Resources/organizeAfter.png)

### Instalation
Delphi Berlin or newer
1. Download [the project](https://github.com/rfrezino/RFindUnit/archive/master.zip), or clone the project.
1. Go to the Packages directory and choose your corresponding version
2. Open the RFindUnit.dproj on your Delphi and right click on the project and install it.

![Example](https://github.com/rfrezino/RFindUnit/blob/master/Resources/InstallationHelp.png)

You can also install [Delphinus package manager](https://github.com/Memnarch/Delphinus/wiki/Installing-Delphinus) and then install RFindUnit as a package there. (Delphinus-Support)

#### Contact
Rodrigo Farias Rezino

rodrigofrezino@gmail.com

#### Referenced Projects
* [DelphiAST](https://github.com/RomanYankovsky/DelphiAST)
* [Log4Pascal](https://github.com/martinusso/log4pascal)
* [Dcu32Int](https://github.com/rfrezino/DCU32INT)

