# SFCToKeYmaera
A tool, coded in Haskell, to translate the XML representation of Sequential Function Chart [See PLCOpen](http://www.plcopen.org/pages/tc6_xml/xml_intro/), which is generated by the PLC IDE [Beremiz](http://www.beremiz.org/), to its corresponding [KeYmaera]([)http://symbolaris.com/info/KeYmaera.html) hybrid program.

This tool is a result of my MSc dissertation entitled "Validation of IEC 61131-3 Programmable Logical Controllers in KeYmaera". The main goal of the dissertation is to use KeYmaera as a tool to validate safety proprieties about a PLC program. This dissertations focused on the SFC language and the overall process of how to convert SFC to a hybrid program.

->![alt tag](http://i21.servimg.com/u/f21/12/93/70/88/sdfdsg10.png)<-

The figure illustrates how the tool actually works, it begins by parsing the XML given by Beremiz and stores the result in a structure called SFC HXT. 

1. This first part was made with the HXT library to parse and input file, hence the name of the structure. 

2. The next step in the diagram is the translation of the SFC HXT to the [SFC+ formal](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.96.8276&rep=rep1&type=pdf), the "+" is introduced by the dissertation to upgrade the language and make the annotation of the dynamics of the system possible, and consequently the annotation of other details.

3. The third step is to translate the SFC+ formal to hybrid automaton which was possible by the [hybrid sequential function charts](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0CBsQFjAAahUKEwjjhN2E_JnIAhXK1BoKHU_tAh8&url=http%3A%2F%2Fwww-i2.informatik.rwth-aachen.de%2Fpub%2Findex.php%3Ftype%3Ddownload%26pub_id%3D787&usg=AFQjCNHm3GCZXX_jslvgYK4krpm_EEPnGw&sig2=TkKSisbj4VfI-fEYxAWNeg) .

4. The final step is possible since hybrid automaton can be directly represented in hybrid program. (see example [here](http://symbolaris.com/info/KeYmaera-guide.html#watertank)).

Please if you detect any bug or incoherence in the program feel free to make a pull request!

