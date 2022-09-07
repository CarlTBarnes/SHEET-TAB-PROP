!This is a Copy of SheetTab.clw with AnyScreen added below in section == Added AnyScreen Here ==    
!Run under AnyScreen in the IDE by clicking at the end of the Toolbar on the "Run Any Screen" button

!SheetTab by Carl Barnes - Copyright 2021 released under the MIT License
!---------------------------------------------------------------------------------------------------
! Sheets and Tabs have many Attributes and Properties.
! The Designer does not offer them all nor display them correctly, until you preview the window.
! This tool allows configuring and viewing them all at runtime.
!
! For more see MSDN on Tab Controls   https://docs.microsoft.com/en-us/windows/win32/controls/tab-controls
! There are messages that get info not available via the RTL like One Tab's RECT or Row Count.
! --------------------------------------------------------------------------------------------------
!
!PROP:Angle     - Changes Text angle on the Ears but it does not look good. Seems like 900 checks UP
!PROP:IMM       - Docs say required to get EVENT:NewSelection on Sheet but not in my tests
!PROP:NoTheme   - Turn Off Sheet Visual Styles ?
!PROP:Tip       - Each TAB can have a TIP() that displays when hovering over Ear even when not selected
!PROP:Hlp HLP() - Each TAB can have a HLP() that as a Parent applies to all controls on the Tab. Good for Topic.htm#Bookmark
!PROP:REQ       - Did you know ... REQ can be on Tab. A tab switch cannot happen if REQ ENTRY are not filled. Good for Wizard?

  PROGRAM
  Include('CBWndPreview.inc')   !https://github.com/CarlTBarnes/WindowPreview
  MAP
SheetTabAssistant   PROCEDURE()
TabLabelEdit        PROCEDURE(LONG TabFEQ, STRING FeqName)
ClaFeqName          PROCEDURE(LONG Feq),STRING
        MODULE('RTL')
ClaFieldNameRTL  PROCEDURE(LONG pFEQ),CSTRING,RAW,NAME('Cla$FIELDNAME'),DLL(dll_mode)
        END
  END
  
  !Project Defines Add: _AnyScreenPresent_=>1
  !== Added AnyScreen Here == Top ==
  PRAGMA('project(#file copy 7z.exe)')      !#PROJECT('None(7z.exe),CopyToOutputDirectory=Always')
  PRAGMA('link(ClaAnyScreen.lib)')          !#PROJECT('ClaAnyScreen.lib')
  PRAGMA('link(ClaAnyScreenNet.lib)')       !#PROJECT('ClaAnyScreenNet.lib')
  PRAGMA('link(ClaI2PDF.lib)')              !#PROJECT('None(ClaI2PDF.dll),CopyToOutputDirectory=Always')  
  PRAGMA('link(ClaASC.lib)')                !#PROJECT('C%V%ASC%X%%L%.LIB') 
  PRAGMA('link(ClaDOS.lib)')                !#PROJECT('C%V%DOS%X%%L%.LIB') 
  PRAGMA('link(ClaOLE.lib)')                !#PROJECT('C%V%OLE%X%%L%.LIB') 
  INCLUDE('AnyScreen.inc')      !Any Screen Include 
  !== Added AnyScreen Here == End ==
  CODE
  AnyScreen:Init()              !Initialize AnyScreen   !== Added AnyScreen Here == 1 more ==
  SheetTabAssistant()
    RETURN
!---------------------------------
SheetTabAssistant   PROCEDURE()

SHT CLASS                                                         !Build ReadProp STRING with data
Prop    PROCEDURE(LONG PropCode, BOOL TF, BOOL IsNegate=0)        !Set Sheet1{PropCode}=TF
RD      PROCEDURE(LONG PropCode, STRING PropName, BYTE No1310=0)  !Read Sheet1{PropCode} and append to ReadProp STRING if not blank
RDA     PROCEDURE(LONG PropCode, STRING PropName, BYTE No1310=0)  !Read Sheet1{PropCode} and append to ReadProp STRING Always / blank too
RD2     PROCEDURE(LONG PropCode, STRING PropName, LONG Index2)    !Read Sheet1{PropCode} and !Read Sheet1{PropCode,2}
RDConCat   PROCEDURE(STRING Txt2Add, BYTE No1310=0)               !Add (concat) Text  to ReadProp STRING
RD1310  PROCEDURE()                                               !Add (concat) 13,10 to ReadProp STRING
    END
TabzRect    STRING(2000)         !debug
ReadProp    STRING(4000)         !Built by SHT CLASS .RDxxx
ReadPropShowALL  BOOL            !SHT.RD shows blanks
SheetCode   STRING(250)          !SHEET() Code as Text

PropNoSheet  SHORT
PropWizard   SHORT
PropSpread   SHORT !TABs are evenly spaced on one line (PROP:SPREAD).
PropLayout   SHORT !TABs Right-to-Left (PROP:LAYOUT)
PropNoTheme  SHORT !It prevents the Operating System theme color from being applied to the SHEET /TAB  (PROP:NoTHEME)

PropHscroll  SHORT !TABs display all on one row instead of multiple rows, no matter how many TABs there are (PROP:HSCROLL). Right and left (or up and down) scroll buttons appear at either end of the TABs to scroll through the TABs.
PropJoin     SHORT !TABs display all on one row instead of multiple rows, no matter how many TABs there are (PROP:JOIN). Right and left (or up and down) scroll buttons appear together at the right (or bottom) end of the TABs to scroll through the TABs.
PropBrokenTabs SHORT !PROP:BrokenTabs may be set to FALSE to turn off the "broken tab" visual effect.

!Dup Property as CHECK
TabLocGroup GROUP,PRE()
PropAbove2    BYTE(1)   !TABs appear above the sheet (PROP:ABOVE). ABOVE is the default position.
PropBelow2    BYTE      !TABs appear below the sheet (PROP:BELOW).
PropLeft2     BYTE      !TABs appear to the left of the sheet (PROP:LEFT).
PropRight2    BYTE      !TABs appear to the right of the sheet (PROP:RIGHT).
            END
TabLocArray BYTE,DIM(4),OVER(TabLocGroup)

PropHscroll2 SHORT !TABs display on one row (PROP:HSCROLL). Right/Left or Up/Down) Scroll buttons appear at each end
PropJoin2    SHORT !TABs display on one row (PROP:JOIN). Right/Left or Up/Down) Scroll buttons appear together at the right (or bottom) end of the TABs to scroll through the TABs.

TextReadsGrp GROUP,PRE()
PropUp2         SHORT !TAB text reads upwards     (PROP:UP)
PropDown2       SHORT !TAB text reads downwards   (PROP:DOWN)
PropUpDown2     SHORT !TAB text reads upside down (PROP:UpsideDown)
             END

TabCount     SHORT(8)
TabNdx       SHORT
TabLocation  SHORT(1)
TabLocPROP   LONG(PROP:Above)
TabLocWidth  SHORT(0)
TabMinWidth  SHORT(0)
TabRectShow  SHORT(0)   !Unhide BOX around tabs showing REC

TextReads    SHORT(1)
Scrolling    SHORT(1)
SheetStyle   SHORT(0)

!FYI: You can increase FONT(,Size) to be 12+ to more visually see the effects better -------------------
Window WINDOW('SHEET TAB - Property and Attribute Explorer'),AT(,,388,263),GRAY,SYSTEM,ICON(ICON:Pick), |
            FONT('Segoe UI',12),RESIZE
        SHEET,AT(3,3,381,175),USE(?SheetMain),BELOW
            TAB('Configure Sheet'),USE(?TabSetProp)
                OPTION('Location'),AT(9,5,74,33),USE(TabLocation),BOXED
                    RADIO('Above'),AT(13,14,35),USE(?TabLocation:Above)
                    RADIO('Below'),AT(13,25,35),USE(?TabLocation:Below)
                    RADIO('Left'),AT(50,14,30),USE(?TabLocation:Left)
                    RADIO('Right'),AT(50,25,30),USE(?TabLocation:Right)
                END
                PROMPT('Tab Wd:'),AT(87,13),USE(?TabLocWidth:Prompt)
                SPIN(@n2),AT(118,13,21,10),USE(TabLocWidth),RANGE(0,70)
                PROMPT('Min Wd:'),AT(87,25),USE(?TabMinWidth:Prompt)
                SPIN(@n2),AT(118,25,21,10),USE(TabMinWidth),RANGE(0,70)
                OPTION('Text Reads'),AT(9,39,62,48),USE(TextReads),BOXED
                    RADIO('Across'),AT(13,47),USE(?TextReads:Default)
                    RADIO('UP'),AT(13,56),USE(?TextReads:UP)
                    RADIO('Down'),AT(13,65),USE(?TextReads:DOWN)
                    RADIO('Upsidedown'),AT(13,74),USE(?TextReads:UpDOWN),TRN
                END
                OPTION('Scrolling Tabs'),AT(78,39,62,39),USE(Scrolling),BOXED
                    RADIO('No'),AT(81,47),USE(?Scrolling:None),TRN
                    RADIO('<< One Line >'),AT(81,56),USE(?Scrolling:Hscroll),TRN
                    RADIO('One Line <<>'),AT(81,65),USE(?Scrolling:Join),TRN
                END
                CHECK('Broken Effect'),AT(78,79),USE(PropBrokenTabs)
                CHECK('Spread Tabs to Fill One Line'),AT(9,91),USE(PropSpread)
                CHECK('No Sheet - No 3D Panel'),AT(9,102),USE(PropNoSheet)
                CHECK('Wizard - No Tab Ears'),AT(9,111),USE(PropWizard)
                CHECK('Flip Tabs Right -to- Left'),AT(9,122),USE(PropLayout)
                CHECK('No OS Theme / Visual Styles'),AT(9,131),USE(PropNoTheme)
                PROMPT('Tab Sheet Style:'),AT(9,144),USE(?Style:Prompt)
                LIST,AT(65,144,66,10),USE(SheetStyle),DROP(9),FROM('Default|#0|Black & White|#1|Colo' & |
                        'red|#2|Squared|#3|Boxed|#4')
                BUTTON('Text...'),AT(147,6,35,12),USE(?TextPickBtn),TIP('Popup to pick from a variet' & |
                        'y of Tab Ear Label text<13,10>to change "Tab1, Tab2..." to Months, Colors o' & |
                        'r other text.')
                BUTTON('Color...'),AT(189,6,35,12),USE(?ColorSheetBtn),TIP('Select SHEET or TAB Ear ' & |
                        'Color (PROP:Color)')
                BUTTON('Icons...'),AT(229,6,35,12),USE(?IconPickBtn),TIP('Add Icons to Tab Ears')
                BUTTON('ReRun'),AT(289,6,30,12),USE(?ReRunBtn),TIP('Run another instance of Sheet Tab')
                PROMPT('Number of Tabs:'),AT(148,152),USE(?TabCount:Prompt)
                SPIN(@n2),AT(207,152,30,10),USE(TabCount),HVSCROLL,RANGE(1,12)
                CHECK('Tab Rectange'),AT(260,152),USE(TabRectShow),TIP('Show Yellow Box over Tab REC' & |
                        'T using PROP:TabRect')
                GROUP,AT(330,12,49,152),USE(?GroupOfPropCHECK),FONT('Consolas')
                    STRING('PROP:'),AT(345,13),USE(?Prop:Heading)
                    CHECK('Above'),AT(334,22),USE(PropAbove2)
                    CHECK('Below'),AT(334,31),USE(PropBelow2)
                    CHECK('Left'),AT(334,40),USE(PropLeft2)
                    CHECK('Right'),AT(334,49),USE(PropRight2)
                    CHECK('UP  '),AT(334,60),USE(PropUp2)
                    CHECK('Down'),AT(334,69),USE(PropDown2)
                    CHECK('UpDown'),AT(334,78),USE(PropUpDown2)
                    CHECK('HScroll'),AT(334,89),USE(PropHscroll2)
                    CHECK('Join'),AT(334,98),USE(PropJoin2)
                    CHECK('Broken'),AT(334,107),USE(PropBrokenTabs,, ?PropBrokenTabs:2)
                    CHECK('Spread'),AT(334,118),USE(PropSpread,, ?PropSpread:2)
                    CHECK('NoSheet'),AT(334,127),USE(PropNoSheet,, ?PropNoSheet:2)
                    CHECK('Wizard'),AT(334,136),USE(PropWizard,, ?PropWizard:2)
                    CHECK('Layout'),AT(334,145),USE(PropLayout,, ?PropLayout:2)
                    CHECK('NoTheme'),AT(334,154),USE(PropNoTheme,, ?PropNoTheme:2)
                END
                GROUP,AT(147,22,177,126),USE(?GROUP1),BOXED,BEVEL(-1,1)
                    SHEET,AT(150,25,170,120),USE(?SHEET1)
                        TAB('Tab&1'),USE(?TAB1)
                        END
                        TAB('Tab&2'),USE(?TAB2)
                        END
                        TAB('Tab&3'),USE(?TAB3)
                        END
                        TAB('Tab&4'),USE(?TAB4)
                        END
                        TAB('Tab&5'),USE(?Tab5)
                        END
                        TAB('Tab&6'),USE(?Tab6)
                        END
                        TAB('Tab&7'),USE(?Tab7)
                        END
                        TAB('Tab&8'),USE(?Tab8)
                        END
                        TAB('Tab&9'),USE(?Tab9),HIDE
                        END
                        TAB('Tab1&0'),USE(?Tab10),HIDE
                        END
                        TAB('Tab1&1'),USE(?Tab11),HIDE
                        END
                        TAB('Tab1&2'),USE(?Tab12),HIDE
                        END
                    END
                    TEXT,AT(189,54,85,50),USE(SheetCode),BOXED,FLAT,FONT('Consolas',9),READONLY
                    BOX,AT(187,111,88,20),USE(?TabRectBox),COLOR(COLOR:Orange),FILL(0C0FFFFH),HIDE, |
                            LINEWIDTH(1)
                END
            END
            TAB('View Sheet Properties'),USE(?TabRedProp)
                TEXT,AT(10,9,366,151),USE(ReadProp),HVSCROLL,FONT('Consolas',10)
                CHECK('Show All PROP'),AT(218,168),USE(ReadPropShowALL),SKIP,TIP('Show All PROPs inc' & |
                        'luding Blanks')
            END
        END
        TEXT,AT(4,182,,81),FULL,USE(TabzRect),BOXED,FLAT,VSCROLL,FONT('Consolas',10)
    END

DOO CLASS                      !----- ROUTINE's replaced by DOO Class -----
PrepareWindow     PROCEDURE()  !After Open(Window) do some configure of window and controls
Prep_Manifest_ON  PROCEDURE()  !Config for Visual Styles if _Manifested_On_=>=On in Style project
BuildSheetCODE    PROCEDURE()  !Build SheetCode Text with Sheet Code for Attrib and PROPs
ReadPropsOfSheet  PROCEDURE()  !Build ReadProp Text by reading all Sheet PROP:s
TabLocationWidth  PROCEDURE()  !Take TabLocWidth input and set Sheet ABOVE( Width ) Prop:ABOVE{,2}=TabLocWidth or BELOW / LEFT / RIGHT
TipsOnControls    PROCEDURE()  !Set Tips on most controls using PROP:Tip, most PROPs have 2 controls with same Tip
ColorPickButton   PROCEDURE()  !Pick color and set ?Tab{PROP:Color}
TextOnTabPick     PROCEDURE()  !Popup to pick Tab('Text') so other than Tab1
IconOnTabPick     PROCEDURE()  !Popup to pick if want Icons on Tabs or not
    END
WndPrvCls   CBWndPreviewClass
    CODE
    OPEN(Window)
    WndPrvCls.Init()
    DOO.PrepareWindow()
    DOO.TipsOnControls()

    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow     ; DOO.BuildSheetCODE()
        OF EVENT:Rejected       ; DISPLAY(?) ; SELECT(?) ; CYCLE
        END
        CASE FIELD()
        OF ?SheetMain   ; IF EVENT()=EVENT:NewSelection AND CHOICE(?SheetMain)=2 THEN DOO.ReadPropsOfSheet().
        OF ?TabLocWidth ; IF EVENT()=EVENT:NewSelection THEN POST(EVENT:Accepted,?).
        OF ?TabMinWidth ; IF EVENT()=EVENT:NewSelection THEN POST(EVENT:Accepted,?).
        OF ?TabCount
            CASE EVENT()
            OF EVENT:NewSelection ; POST(EVENT:Accepted,?TabCount)
            END
        END
        CASE ACCEPTED()
        OF ?TabCount
               IF TabCount < 1 OR TabCount > 12 THEN TabCount=8.
               LOOP TabNdx=1 TO 12
                   (?Tab1+TabNdx-1){PROP:Hide}=CHOOSE(TabNdx>TabCount)
               END
        OF ?PropNoSheet OROF ?PropNoSheet:2 ; SHT.Prop(PROP:NoSheet,PropNoSheet)
        OF ?PropWizard  OROF ?PropWizard:2  ; SHT.Prop(PROP:Wizard, PropWizard)
        OF ?PropSpread  OROF ?PropSpread:2  ; SHT.Prop(PROP:Spread ,PropSpread)
        OF ?PropLayout  OROF ?PropLayout:2  ; SHT.Prop(PROP:Layout ,PropLayout)
        OF ?PropNoTheme OROF ?PropNoTheme:2 ; SHT.Prop(PROP:NoTheme,PropNoTheme)

        OF ?TabLocation ; TabLocPROP = CHOOSE(TabLocation,PROP:Above,PROP:Below,PROP:Left,PROP:Right)
                          SHT.Prop(TabLocPROP ,1  )
                          CLEAR(TabLocArray[]) ; TabLocArray[TabLocation]=1
                          IF TabLocWidth THEN DOO.TabLocationWidth().
                          DISPLAY
        OF ?TabLocWidth ; DOO.TabLocationWidth()
        OF ?TabMinWidth ; ?Sheet1{PROP:MinWidth}=TabMinWidth
        OF ?TabRectShow ; ?TabRectBox{PROP:Hide}=1-TabRectShow
        OF ?TextReads   ; CLEAR(TextReadsGrp)
                          CASE TextReads
                          OF 1 ; SHT.Prop(PROP:UpsideDown,0)    !Default is Up and Dn off
                          OF 2 ; SHT.Prop(PROP:Up, 1)           ; PropUp2=1
                          OF 3 ; SHT.Prop(PROP:Down, 1)         ; PropDown2=1
                          OF 4 ; SHT.Prop(PROP:UpsideDown, 1)   ; PropUpDown2=1
                          END
                          DISPLAY

        OF ?Scrolling   ; IF PropHscroll THEN SHT.Prop(PROP:Hscroll,0). ; PropHscroll=0 ; PropHscroll2=0
                          IF PropJoin    THEN SHT.Prop(PROP:Join, 0).   ; PropJoin=0    ; PropJoin2=0
                          CASE Scrolling
                          OF 2 ; SHT.Prop(PROP:Hscroll,1) ; PropHscroll=1 ; PropHscroll2=1
                          OF 3 ; SHT.Prop(PROP:Join, 1)   ; PropJoin=1    ; PropJoin2=1
                          END
                          DISPLAY
        OF ?PropBrokenTabs OROF ?PropBrokenTabs:2 ; SHT.Prop(Prop:BrokenTabs, PropBrokenTabs)

        OF ?SheetStyle
            ?SHEET1{PROP:TabSheetStyle} = SheetStyle ; DISPLAY

        OF ?ColorSheetBtn ; DOO.ColorPickButton()
        OF ?TextPickBtn   ; DOO.TextOnTabPick()
        OF ?IconPickBtn   ; DOO.IconOnTabPick()
        OF ?ReRunBtn      ; RUN(COMMAND('0')) ; CYCLE
        OF ?ReadPropShowALL ; DOO.ReadPropsOfSheet()

       !---------------------- Duplicate CHECK boxes on right sync to OPTION Radios ------------------
        OF   ?PropAbove2
        OROF ?PropBelow2
        OROF ?PropLeft2
        OROF ?PropRight2  ; TabLocation = FIELD() - ?PropAbove2 + 1
                            IF ~FIELD(){PROP:Checked} THEN  TabLocation=1.  !if uncheck go with default of Above
                            POST(EVENT:Accepted, ?TabLocation)  ; CYCLE

        OF ?PropUp2      ; TextReads=CHOOSE(~PropUp2     ,1,2) ; POST(EVENT:Accepted, ?TextReads)  ; CYCLE
        OF ?PropDown2    ; TextReads=CHOOSE(~PropDown2   ,1,3) ; POST(EVENT:Accepted, ?TextReads)  ; CYCLE
        OF ?PropUpDown2  ; TextReads=CHOOSE(~PropUpDown2 ,1,4) ; POST(EVENT:Accepted, ?TextReads)  ; CYCLE
        OF ?PropHscroll2 ; Scrolling=CHOOSE(~PropHscroll2,1,2) ; POST(EVENT:Accepted, ?Scrolling)  ; CYCLE
        OF ?PropJoin2    ; Scrolling=CHOOSE(~PropJoin2   ,1,3) ; POST(EVENT:Accepted, ?Scrolling)  ; CYCLE

        END !Case Field
        IF ACCEPTED() THEN         !Something changed
           DOO.BuildSheetCODE()    !If did not CYCLE then Accepted recalcs sheet area
        END
    END !Accept
    RETURN

!----------------------------------------------------
DOO.PrepareWindow PROCEDURE()
  CODE
    0{PROP:text}=clip(0{PROP:text}) &' - Clarion ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    0{PROP:MinWidth}=0{PROP:Width}
    0{PROP:MaxWidth}=0{PROP:Width}
    0{PROP:MinHeight}=?TABZRECT{PROP:YPos}-1
    0{PROP:MaxHeight}=0{PROP:Height} * 1.5
    ?SheetMain{PROP:TabSheetStyle}=TabStyle:BlackAndWhite   ! Style=1

        COMPILE('!** Manifest ON **',_Manifested_On_)   !Project has: _Manifested_On_=>1
    DOO.Prep_Manifest_ON()
                 !** Manifest ON **

    PropBrokenTabs=?Sheet1{Prop:BrokenTabs}  !RTL defaults to True
    PropLayout=?Sheet1{PROP:Layout}          !May have set in Code ... unlikely
    PropNoTheme=?Sheet1{PROP:NoTheme}        !May have set below


DOO.Prep_Manifest_ON PROCEDURE()
F LONG(0)
    CODE
    !Did not work: IF ~SYSTEM{PROP:ThemeActive} THEN EXIT.  !Was there a Manifest

!   ?SheetMain{PROP:NoTheme}=1
!    ?Sheet1{PROP:NoTheme}=1
    LOOP
        F=0{PROP:NExtField,F} ; IF ~F THEN BREAK.
        CASE F
        OF ?TabRectBox
            CYCLE
        END
        CASE F{PROP:Type}
        OF CREATE:text ; CYCLE
        END
        F{PROP:Trn}=1
    END
    RETURN

!----------------------------------------------------
DOO.BuildSheetCODE PROCEDURE()
StyleCode   PSTRING(100)
S1  LONG

TbRect GROUP,PRE(TB) !Tab Ears                          SheRect   PanRect  Panel (non tab ears)
ULX         LONG     !Tb:ULX  !Upper-Left X             Sh:ULX    Pn:ULX
ULY         LONG     !Tb:ULY  !Upper-Left Y             Sh:ULY    Pn:ULY
LRX         LONG     !Tb:LRX  !Lower-Right X            Sh:LRX    Pn:LRX
LRY         LONG     !Tb:LRY  !Lower-Right Y            Sh:LRY    Pn:LRY
RX          LONG     !Tb:RX   !Rect X = ULX + Sh:RX-1   Sh:RX     Pn:RX
RY          LONG     !Tb:RY   !Rect Y = ULY + Sh:RY-1   Sh:RY     Pn:RY
RW          LONG     !Tb:RW   !Rect Width               Sh:RW     Pn:RW
RH          LONG     !Tb:RH   !Rect Height              Sh:RH     Pn:RH
        END

SheRect GROUP(TbRect),PRE(Sh)    !Sheet Original RECT
        END
SheCentrX LONG    !Center of Sheet
SheCentrY LONG    !Center of Sheet
PanRect GROUP(TbRect),PRE(Pn)
        END
PanzRect    STRING(200)
    CODE
    IF ACCEPTED()=?SheetCode THEN RETURN.  !did I edit this debug TEXT
    S1=?Sheet1

    IF SheetStyle THEN
        StyleCode ='<13,10>?Sheet{{PROP:TabSheetStyle}=' & |
                    'TabStyle:' & CHOOSE(SheetStyle,'BlackAndWhite','Colored','Squared','Boxed','Future' & SheetStyle) & |
                    ' ! Style=' & SheetStyle
               !CHOOSE(SheetStyle<1,'','<13,10>?Sheet{{PROP:TabSheetStyle}=' & SheetStyle) & |
    END

    SheetCode='SHEET,AT(,,,)' & |  !3,3,' & ?Sheet1{PROP:Width} &','& ?Sheet1{PROP:Width} &')'& |
             CHOOSE(TabLocation=1 AND ~TabLocWidth,'', CHOOSE(TabLocation,',ABOVE',',BELOW',',LEFT',',RIGHT')) & |
             CHOOSE(TabLocWidth=0,'','(' & TabLocWidth & ')') & |
             CHOOSE(TextReads,'',', UP',', DOWN',', UP,DOWN') & |
             CHOOSE(Scrolling,'',', HSCROLL',',JOIN') & |
             CHOOSE(~PropSpread,'',', SPREAD') & |
             CHOOSE(~PropWizard,'',', WIZARD') & |
             CHOOSE(~PropNoSheet,'',', NOSHEET') &'  '& |  
             '<13,10>' & |
             CHOOSE(1=PropBrokenTabs OR Scrolling<2,'','<13,10>?Sheet{{PROP:BrokenTabs}=False') & |
             CHOOSE(~PropLayout,'','<13,10>?Sheet{{PROP:Layout}=1') & |
             CHOOSE(~PropNoTheme,'','<13,10>?Sheet{{PROP:NoTheme}=1') & |
             CHOOSE(~TabMinWidth,'','<13,10>?Sheet{{PROP:MinWidth}='& TabMinWidth) & |
             StyleCode & |
             '<13,10>'
    GETPOSITION(?Sheet1, Sh:RX,Sh:RY,Sh:RW,Sh:RH)
      ! TabzRect = 'hello' & SheetCode ; display ; exit
      TB:ULX = S1{PROP:TabRect,1}   !Upper-Left  X
      TB:ULY = S1{PROP:TabRect,2}   !Upper-Left  Y
      TB:LRX = S1{PROP:TabRect,3}   !Lower-Right X
      TB:LRY = S1{PROP:TabRect,4}   !Lower-Right Y

!The RECT is AT(1,1) not sure if (0,0) is Upper-Left means the RECT is a tiny bit in
!      TB:RX  = TB:ULX + Sh:RX -1    !Rect X = ULX     !PN:RX
!      TB:RY  = TB:ULY + Sh:RY -1    !Rect Y = ULY     !PN:RY
!      TB:RW  = TB:LRX - TB:ULX      !Rect Width       !PN:RW
!      TB:RH  = TB:LRY - TB:ULY      !Rect Height      !PN:RH

      TB:RX  = TB:ULX + Sh:RX       !Rect X = ULX     !PN:RX       !Assume (0,0) is base so (1,1) is tiny bit in
      TB:RY  = TB:ULY + Sh:RY       !Rect Y = ULY     !PN:RY
      TB:RW  = TB:LRX - TB:ULX + 1  !Rect Width       !PN:RW
      TB:RH  = TB:LRY - TB:ULY + 1  !Rect Height      !PN:RH

    SheCentrX = Sh:RW / 2    !Center of Sheet
    SheCentrY = Sh:RH / 2    !Center of Sheet

    TabzRect = CHOOSE(~PropNoSheet,'','NOSHEET - ') & CHOOSE(TabLocation,'PROP:Above','PROP:Below','PROP:Left','PROP:Right')  & |
                ' - ' & CHOOSE(TextReads,'Across','PROP:Up','PROP:Down','PROP:UpsideDown') & |
                ' - PROP:TabRows=' & ?Sheet1{PROP:TabRows} & ' <13,10>' & |
              '<13,10>Sheet AT     (X,Y, W,H) = (' &  Sh:RX  &' , '& Sh:RY  &' , '& Sh:RW  &' , '&  Sh:RH &')' & |
              '<13,10>PROP:TabRect (L,T, R,B) = (' &  TB:ULX &' , '& TB:ULY &' , '& TB:LRX &' , '& TB:LRY &')' & |
           | !'<13,10>PROP:TabRect (X,Y, W,H) = (' &  TB:RX  &' , '& TB:RY  &' , '& TB:RW  &' , '& TB:RH  &')' & |
              '<13,10>Tab RECT AT  (X,Y, W,H) = (' &  TB:RX  &' , '& TB:RY  &' , '& TB:RW  &' , '& TB:RH  &')' & |
              '<13,10>Note: A RECT (Left,Top Right,Bottom)  Width=Right-Left Height=Bottom-Top' & |
              '<13,10>Sheet Center X=' & SheCentrX & '  Sheet Center Y=' & SheCentrY & |
              ' <13,10><13,10>'
!   SETPOSITION(?TabRectBox, TB:RX, TB:RY, TB:RW, TB:RH)         !Position Yellow Rect over Tab RECT
    SETPOSITION(?TabRectBox, TB:RX-1, TB:RY-1, TB:RW+2, TB:RH+2) !Looks better at -1,+2 Position Yellow Rect over Tab RECT

    PanRect = SheRect
!    Pn:RX += 10 ; Pn:RW -= 20
!    Pn:RY += 10 ; Pn:RH -= 20

  !  IF (TabLocation=1 AND ~PropNoSheet)  OR  (TabLocation=2 AND PropNoSheet) THEN !Upper Left
    IF Tb:LRX > SheCentrX AND  Tb:LRY < SheCentrY THEN  !ABOVE? if X ends Right and Y end Above
        Pn:RY += Tb:RH
        Pn:RH -= Tb:RH
        PanzRect = 'ABOVE'

    ELSIF Tb:LRX > SheCentrX AND  Tb:ULY > SheCentrY THEN  !BELOW? if X ends Right and Y end Below
       ! Pn:RY -= Tb:RH
        Pn:RH -= Tb:RH
        PanzRect = 'BELOW'

    ELSIF Tb:LRX < SheCentrX AND  Tb:LRY > SheCentrY THEN  !LEFT? if X ends Left and Y end Above
        Pn:RX += Tb:RW
        Pn:RW -= Tb:RW
        PanzRect = 'LEFT'

    ELSIF Tb:LRX > SheCentrX AND  Tb:LRY > SheCentrY THEN  !RIGHT?
       ! Pn:RX += Tb:RW
        Pn:RW -= Tb:RW
        PanzRect = 'RIGHT'
     ELSE
        TabzRect=CLIP(TabzRect)&'<13,10> ??? ELSE Failed tabs location in BuildSheetCODE() ??? '
    END

    Pn:RX += 10 ; Pn:RW -= 20 ! ; IF Pn:RX < Sh:RX + 10 THEN  Pn:RX = Sh:RX + 10.
    Pn:RY += 10 ; Pn:RH -= 20 ! ; IF Pn:RY < Sh:RY + 10 THEN  Pn:RY = Sh:RY + 10.
    !-- With Left / Right NoSheet the Tab Ears fill the tab area
  !  IF Pn:RH < 10 THEN Pn:RH = Sh:RH - 20.  !Left NoSheet Tabs fill Sheet so must resize
    IF PanzRect THEN
       HIDE(?SheetCode) ; DISPLAY
       SETPOSITION(?SheetCode, Pn:RX, Pn:RY, Pn:RW, Pn:RH )
       UNHIDE(?SheetCode)
       TabzRect=CLIP(TabzRect)& CLIP(PanzRect) &  |
          '<13,10>Text Panel (X,Y, W,H) = (' &  Pn:RX  &' , '& Pn:RY  &' , '& Pn:RW  &' , '& Pn:RH  &')'
    END
    DISPLAY
    RETURN

!----------------------------------------------------
DOO.ReadPropsOfSheet PROCEDURE()
S1      LONG,AUTO
TbFeq   LONG,AUTO
TbNdx   USHORT,AUTO
    CODE
    DOO.BuildSheetCODE()
    S1=?Sheet1
    ReadProp= 'Control Statement: ' & CLIP(SheetCode) &'<13,10>-{41}<13,10>' &|
            'Properties read from ?SHEET as configured:<13,10>'

    SHT.RD2(Prop:Above  , 'PROP:Above',2)
    SHT.RD2(Prop:Below  , 'PROP:Below',2)
    SHT.RD2(Prop:Left   , 'PROP:Left',2)
    SHT.RD2(Prop:Right  , 'PROP:Right',2)

    SHT.RD(Prop:UP      , 'PROP:UP')
    SHT.RD(Prop:Down    , 'PROP:Down')
    SHT.RD(PROP:UpsideDown   , 'PROP:UpsideDown   ')

    SHT.RD(Prop:Spread , 'PROP:Spread  -- Tab Ear Width Fills Space')
    SHT.RD(Prop:HScroll, 'PROP:HScroll -- 1 Row, 1 Button on Both Ends')
    SHT.RD(Prop:Join   , 'PROP:Join    -- 1 Row, 2 Buttons on One End')

    SHT.RD(PROP:BrokenTabs, 'PROP:BrokenTabs')
    SHT.RD(Prop:NoSheet, 'PROP:NoSheet')
    SHT.RD(Prop:Wizard , 'PROP:Wizard -- No Tab Ears')
    SHT.RD(Prop:Layout , 'PROP:Layout -- Right-to-Left ')
    SHT.RD(PROP:TabSheetStyle , 'PROP:TabSheetStyle ')
    SHT.RD(PROP:NoTheme , 'PROP:NoTheme -- Turn Off OS Sheet Theme / Visual Styles')
    SHT.RD(Prop:MinWidth, 'PROP:MinWidth -- Minimum Width of Tab Ear')
    SHT.RD1310()

    SHT.RDconcat(TabCount & '=PROP:NumTabs' ) ! visible, With Hidden=' & ?Sheet1{PROP:NumTabs} ')
    SHT.RDA(PROP:TabRows , 'PROP:TabRows ')
    SHT.RDA(PROP:IMM,'PROP:IMM -- Help says required for Event:NewSelection, but NOT in my tests' )
    SHT.RD1310()

    SHT.RDconcat(S1{PROP:Selected}  & '=PROP:Selected -- CHOICE(Sheet)='& CHOICE(S1) )
    SHT.RDconcat(S1{PROP:ChoiceFEQ} & '=PROP:ChoiceFEQ -- Sheet{{PROP:Child,CHOICE(Sheet)} = '& S1{PROP:Child,CHOICE(S1)} & |
                                            ' = ?Tab' & (S1{PROP:ChoiceFEQ}){PROP:ChildIndex} )
    SHT.RD1310()
    SHT.RDconcat('PROP:AT      (X,Y,W,H) = (' &  s1{PROP:Xpos} &' , '&  s1{PROP:Ypos} &' , '& s1{PROP:Width} &' , '& s1{PROP:Height} &')' )
    SHT.RDconcat('PROP:TabRect (X,Y,W,H) = (' &  s1{PROP:TabRect,1} &' , '&  s1{PROP:TabRect,2} &' , '& s1{PROP:TabRect,3} &' , '& s1{PROP:TabRect,4} &')' )
    SHT.RDconcat('             Location of Tab Ears Rectange relative to AT() of Sheet. Normally AT(1,1) unless Left, Bottom, HScroll ')

    SHT.RD1310()
    SHT.RDconcat('PROP:FontName=' & S1{PROP:FontName} &'  FontSize=' & S1{PROP:FontSize} &'  FontStyle=' & S1{PROP:FontStyle} &'  FontColor=' & S1{PROP:FontColor} )
    !SHT.RD( , ' ')

    SHT.RD1310()
    SHT.RDconcat('-{40}<13,10>')
    SHT.RDconcat('?Sheet1 FEQ='& ?Sheet1 )
    SHT.RDconcat('?Tab1   FEQ='& ?Tab1 &' ?Tab1{{PROP:Parent}='& ?Tab1{PROP:Parent}  &' ?Tab1{{PROP:ChildIndex}='& ?Tab1{PROP:ChildIndex})
    LOOP TbNdx=1 TO ?Sheet1{PROP:NumTabs}
         TbFEQ=?Sheet1{PROP:Child,TbNdx}
         SHT.RDconcat('?Sheet1{{PROP:Child,'& TbNdx &'}='& ?Sheet1{PROP:Child,TbNdx} &' = ?Tab'& TbNdx & |
                        ' - PROP:ChildIndex='& TbFEQ{PROP:ChildIndex} & |
                        ' - PROP:Text="'& TbFEQ{PROP:Text} &'"'& |
                        ' - PROP:Value="'& TbFEQ{PROP:Value} &'"')   ! & ' - Hide='& TbFEQ{PROP:Hide} )
    END

    DISPLAY
    RETURN
!----------------------------
DOO.TabLocationWidth PROCEDURE()
  CODE
    ?Sheet1{(TabLocPROP),2}=TabLocWidth ; SHT.Prop(TabLocPROP ,1  )
    RETURN
!----------------------------
DOO.TipsOnControls PROCEDURE()
  CODE

    ?PropSpread {PROP:Tip}='TABs are evenly spaced when they fit on one line (PROP:SPREAD)'
    ?PropNoSheet{PROP:Tip}='TABs display without a visible 3D Panel (PROP:NOSHEET).' & |
                           '<13,10>Tab Ear location and orientation reverses<13,10>i.e. Below appears Above with Ears flipped.'
    ?PropWizard {PROP:Tip}='SHEETs TAB controls do not appear (PROP:WIZARD).' & |
                    '<13,10>For a FLAT look set PROP:NoSheet=True to remove the 3D Panel,' & |
                    '<13,10>then you can create the panel(s) style and colors you desire.' & |
                    '<13,10><13,10>The user moves from TAB to TAB under program control, ' & |
                    '<13,10>i.e. Next / Previous buttons that SELECT(?TabX).'
    ?PropLayout{PROP:Tip}='TABs display flipped Right-to-Left wih first tab on Right edge (PROP:LAYOUT).' & |
                   '<13,10>A style of (1) essentially "flips" the window controls display as a mirror image'&  |
                   '<13,10>of the layout specified in the Window Formatter.' & |
                   '<13,10>Default field navigation moves from Right to Left.'
    ?PropNoTheme{PROP:Tip}='Prevents Operating System theme color from being applied to the SHEET /TAB controls ' & |
                    '<13,10>allowing you to set the Colors. (PROP:NoTHEME)'&  |
                    '<13,10>'&  |
                    '<13,10>The Templates provide support to apply PROP:NoTheme to all SHEET controls in the Application.'&  |
                    '<13,10>To activate the template support go to Global Properties->Actions->App Settings->'&  |
                    '<13,10>->Application Manifest and turn on "SHEET controls do not use the OS Theme color".'

    ?PropSpread:2 {PROP:Tip} = ?PropSpread{PROP:Tip}
    ?PropNoSheet:2{PROP:Tip} = ?PropNoSheet{PROP:Tip}
    ?PropWizard:2 {PROP:Tip} = ?PropWizard{PROP:Tip}
    ?PropLayout:2 {PROP:Tip} = ?PropLayout{PROP:Tip}
    ?PropNoTheme:2{PROP:Tip} = ?PropNoTheme{PROP:Tip}

    ?Scrolling:None{PROP:Tip}   ='No Tab Scrolling Buttons.<13,10>Tabs display on Multiple Rows when required.<13,10>PROP:TabRows returns number of Rows.'
    ?Scrolling:Hscroll{PROP:Tip}='PROP:HSCROLL<13,10>TABs display all on One Row instead of Multiple Rows' & | !, <13,10>no matter how many TABs. ' & |
                          '<13,10>Right and Left (or Up and Down) scroll buttons appear at ' & |
                          '<13,10>BOTH ENDS of the TABs to scroll through the TABs.'
    ?Scrolling:Join   {PROP:Tip}='PROP:JOIN<13,10>TABs display all on One Row instead of Multiple Rows.' & | !, <13,10>no matter how many TABs. ' & |
                          '<13,10>Right and Left (or Up and Down) scroll buttons appear together' & |
                          '<13,10>AT THE RIGHT (or bottom) of the TABs to scroll through the TABs.'

    ?PropHscroll2{PROP:Tip} = ?Scrolling:Hscroll{PROP:Tip}
    ?PropJoin2   {PROP:Tip} = ?Scrolling:Join   {PROP:Tip}

    ?PropBrokenTabs{PROP:Tip}='Uncheck (False) to turn off the "broken tab" visual effect<13,10>when scrolling tabs. (PROP:BrokenTabs)'
    ?PropBrokenTabs:2{PROP:Tip}=?PropBrokenTabs{PROP:Tip}

      ?PropUp2{PROP:Tip} ='TAB text is vertical reading upwards (PROP:UP)'
    ?PropDown2{PROP:Tip} ='TAB text is vertical reading downwards (PROP:Down)'
   ?PropUpDown2{PROP:Tip}='TAB text is inverted reading upside down (PROP:UpsideDown)' & |
                          '<13,10>Add both UP and DOWN to SHEET declararion.'
    ?TextReads:UP {PROP:Tip}    = ?PropUp2{PROP:Tip}
    ?TextReads:DOWN{PROP:Tip}   = ?PropDown2{PROP:Tip}
    ?TextReads:UpDOWN{PROP:Tip} = ?PropUpDown2{PROP:Tip}
    ?TextReads:Default{PROP:Tip} = 'TAB text is horizontal reading across<13,10>PROP:UP and PROP:DOWN both false.'

    ?TabLocation:ABOVE{PROP:Tip}='TABs appear above the sheet (PROP:ABOVE).<13,10>This is the default position.'
    ?TabLocation:BELOW{PROP:Tip}='TABs appear below the sheet (PROP:BELOW)'
    ?TabLocation:LEFT{PROP:Tip} ='TABs appear to the left of the sheet (PROP:LEFT)'
    ?TabLocation:RIGHT{PROP:Tip}='TABs appear to the right of the sheet (PROP:RIGHT)'

    ?PropAbove2{PROP:Tip} = ?TabLocation:ABOVE{PROP:Tip}
    ?PropBelow2{PROP:Tip} = ?TabLocation:BELOW{PROP:Tip}
    ?PropLeft2 {PROP:Tip} = ?TabLocation:LEFT {PROP:Tip}
    ?PropRight2{PROP:Tip} = ?TabLocation:RIGHT{PROP:Tip}

    ?TabLocWidth{PROP:Tip}='The Fixed Width of the TAB ears in Dialog Units. All are the same Width.' & |
                           '<13,10>Property is Index {{,2} of PROP: Above, Below, Left, Right, '& |
                           '<13,10>or PROP:AboveSize, PROP:BelowSize, PROP:LeftOffset, PROP:RightOffset' & |
                           '<13,10>Tab Label (Text) longer than the Fixed Width is truncated.'

    ?TabMinWidth{PROP:Tip}='The Minimum Width of the TAB Ears in Dialog Units. (PROP:MinWidth)' & |
                           '<13,10>Ignored if a Fixed Width is set or PROP:Spread is On.' & |
                           '<13,10><13,10>Alternative: Add spaces on end of TAB(''Label ...'') to make it wider.'

    ?SheetStyle{PROP:Tip}='PROP:TabSheetStyle<13,10>Specifies the Visual Style of the Tab Ears with options' & |
                          '<13,10>Office 2003 style trapezoid ears in ' & |
                          '<13,10>1=Black & White; 2=Colored; 3=Squared or 4=Boxed. 0=Default'

    ?Prop:Heading{PROP:Tip}='Below are all the SHEET{{ PROP: }''s that are True/False.' & |  !SYI a STRING can have a Tip
                          '<13,10>The prefix "PROP:" has been omitted, e.g. "Above" is "PROP:Above"' & |
                          '<13,10>These can be checked or most have a way to select on the left side.'

!-----------------------------------------------------
DOO.ColorPickButton PROCEDURE()
Clr          LONG(0)
ColorFEQ     LONG,AUTO
TabTxt       PSTRING(10),AUTO
    CODE
    ColorFEQ = ?Sheet1{PROP:ChoiceFEQ}
    TabTxt = 'Tab ' & CHOICE(?Sheet1)
    CASE POPUP('Pick Sheet Color...|Pick '   & TabTxt & ' Color' & |
            '|-|Remove Sheet Color |Remove ' & TabTxt & ' Color' )
    OF 1   ; ColorFEQ = ?Sheet1 ; TabTxt = 'Sheet'
    OF 2
    OF   3 ; ColorFEQ = ?Sheet1     !Note Tricky the OROF also executes
    OROF 4 ; Clr = COLOR:None
    ELSE   ; RETURN
    END
    IF Clr <> COLOR:None THEN
       IF ~COLORDIALOG('Select Color for ' & TabTxt , Clr) THEN RETURN.
    END
    ColorFEQ{PROP:Color} = Clr
    DISPLAY
    RETURN
!---------------------------------------------------------
DOO.TextOnTabPick PROCEDURE()
PopNo   USHORT,AUTO
TbFEQ   LONG,AUTO
TbNdx   USHORT,AUTO
TabTxt  PSTRING(10),AUTO
    CODE
    TbFEQ  = ?Sheet1{PROP:ChoiceFEQ}
    TabTxt = 'Tab ' & CHOICE(?Sheet1)
    PopNo = POPUP('Enter '& TabTxt &' Label Text|-' & |
                '|Numbers (Tab 1, Tab 2...)|Nato Alphabet|Number Names|Month Names Long|Month Names Short' & |
                '|Colors|Controls|Paper|Character Sets|Christmas|A BB CCC DDDD ... MinWidth Test')
    CASE PopNo
    OF 0 ; RETURN
    OF 1
        TabLabelEdit(TbFEQ,TabTxt)
        RETURN
    ELSE
        PopNo -= 1
    END
    LOOP TbNdx=1 TO ?Sheet1{PROP:NumTabs}
         TbFEQ=?Sheet1{PROP:Child,TbNdx}
         EXECUTE PopNo
           TbFEQ{PROP:Text}='Tab' & CHOOSE(TbNdx<=9,'&' & TbNdx, '1&' & TbNdx%10)
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'&Alfa','Bravo','&Charlie','&Delta','&Echo','&Foxtrot','&Golf','&Hotel','&India','&Juliett','&Kilo','&Lima')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'One','Two','Three','Four','Five','Six','Seven','Eight','Nine','Ten','Eleven','Twelve')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'January','February','March','April','May','June','July','August','September','October','November','December')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'Jan','Feb','March','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'&Blue','&Fuchsia','&Green','&Lime','&Maroon','&Navy','&Orange','&Purple','&Red','&Silver','&Teal','&Yellow')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'Button','Check','Combo','Entry','Image','List','Option','Prompt','Region','Sheet','Spin','Text')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'A3','A4','A5','Executive','Folio','Ledger','Legal','Letter','Note','Quarto','Statement','Tabloid')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'&Arabic','&Baltic','&Cyrillic','&Greek','&Hangeul','Hebre&w','&Johab','Shift J&IS','&Symbol','&Thai','T&urkish','&Vietnamese')
           TbFEQ{PROP:Text}=CHOOSE(TbNdx,'&1 Partridge','&2 Doves','&3 Hens','&4 Birds','&5 Rings','&6 Geese','&7 Swans','&8 Maids','&9 Ladies','1&0 Lords','11 &Pipers','12 &Drummers')
           TbFEQ{PROP:Text}=ALL(SUB('ABCDEFGHIJKLMNO',TbNdx,1),TbNdx)
         END
         TbFEQ{PROP:Tip}='Tab' & TbNdx
    END
    DISPLAY
    RETURN
!---------------------------------------------------------
DOO.IconOnTabPick PROCEDURE()
PopNo   USHORT,AUTO
TbFeq   LONG,AUTO
TbNdx   USHORT,AUTO
IconEqt ULONG(7F0102FFh)             !1st   ICON:Pick EQUATE ('<0FFH,02H,01H,7FH>') = 7F0102FFh
IconStr STRING(4),OVER(IconEqt)      !Next  ICON:Save EQUATE ('<0FFH,02H,02H,7FH>') = 7F0202FFh
IcoIncr ULONG(00010000h)             !                                                   10000h increment
    CODE
    PopNo = POPUP('Show Icons on Tabs' & |              !#1
                 '|None Icons on Tabs (ICON:None)' & |    !#2
               '|-|Blank Prop:Icon = "" (shuts down)')

    IF ~PopNo THEN RETURN.
    LOOP TbNdx=1 TO ?Sheet1{PROP:NumTabs}     !   LOOP TbNdx=?Sheet1{PROP:NumTabs}  TO 1 BY -1
         TbFEQ=?Sheet1{PROP:Child,TbNdx}
!         IF TbFEQ{PROP:Hide} THEN CYCLE.
         CASE PopNo
         OF 1 ;  TbFEQ{PROP:Icon}=IconStr
         OF 2 ;  TbFEQ{PROP:Icon}=ICON:None
         OF 3 ;  TbFEQ{PROP:Icon}=''                !this shuts down the program. Sent to SV
         END
         IconEqt += IcoIncr     !Next  ICON:Save EQUATE('<0FFH,02H,02H,7FH>') ... 12= ICON:Clarion EQUATE('<0FFH,02H,0CH,7FH>')
    END
    DISPLAY
    RETURN

!=============== Class Code ==============
SHT.Prop PROCEDURE(LONG PropCode, BOOL TF, BOOL IsNegate=0) !, BOOL NoDisplay=0)
    CODE
    IF IsNegate THEN TF=CHOOSE(~TF).
    ?SHEET1{PropCode}=TF
    DISPLAY
    RETURN
!===================================
SHT.RD      PROCEDURE(LONG PropCode, STRING PropName, BYTE No1310=0)  !Add to Read Property
pValue  PSTRING(128)
    CODE
    pValue = ?Sheet1{PropCode}
    IF ~ReadPropShowALL AND ~pValue THEN RETURN.
    ReadProp=CLIP(ReadProp) & pValue & '=' & PropName  & CHOOSE(~No1310,'<13,10>','')
    RETURN
!===================================
SHT.RDA     PROCEDURE(LONG PropCode, STRING PropName, BYTE No1310=0)  !Always Output even if False
pValue  PSTRING(128)
    CODE
    pValue = ?Sheet1{PropCode}
    !No IF ~pValue THEN RETURN.
    ReadProp=CLIP(ReadProp) & pValue & '=' & PropName  & CHOOSE(~No1310,'<13,10>','')
    RETURN
!---------------------------------------
SHT.RD2     PROCEDURE(LONG PropCode, STRING PropName, LONG Index2)
pValue  PSTRING(128)
pValue2  PSTRING(128)
    CODE
    pValue = ?Sheet1{PropCode}
    pValue2 = ?Sheet1{PropCode, Index2}
    IF ~ReadPropShowALL AND ~pValue THEN RETURN.
    ReadProp=CLIP(ReadProp) & pValue & '=' & PropName &'  '& |
             CHOOSE(~pValue2 AND ~ReadPropShowALL,'','  Index {{,' & Index2 &'}=' & pValue2) & '<13,10>'
    RETURN
!---------------------------------------
SHT.RDConCat   PROCEDURE(STRING Txt2Add, BYTE No1310=0)  !Add Text to ReadProp
pValue  PSTRING(128)
    CODE
    ReadProp=CLIP(ReadProp) & CLIP(Txt2Add) & CHOOSE(~No1310,'<13,10>','')
    RETURN
!---------------------------------------

SHT.RD1310  PROCEDURE()
    CODE
    ReadProp=CLIP(ReadProp) & '<13,10>'
    RETURN

!================================================================================
TabLabelEdit PROCEDURE(LONG TabFEQ, STRING FeqName)
Txt     CSTRING(33),AUTO    !(Trailing Spaces Allowed) did not work with PROP
Undo    CSTRING(33),AUTO
Ok2Set  BYTE

EditWn WINDOW('Edit Tab Label'),AT(,,218,56),GRAY,SYSTEM,FONT('Segoe UI',9)
        STRING('Tab Label Text:'),AT(9,3),USE(?Value:Pmt)
        ENTRY(@s32),AT(9,15,200,18),USE(Txt),FONT('Consolas',12)
        BUTTON('&Set Label'),AT(39,38,37),USE(?SetBtn),DEFAULT,TIP('Set Control PROP to below value')
        BUTTON('&Undo'),AT(97,38,35),USE(?UndoBtn)
        BUTTON('&Close'),AT(157,38,35),USE(?CloseBtn),STD(STD:Close)
    END
P LONG,DIM(4),STATIC
X LONG,AUTO
PROP:: LONG,AUTO
    CODE
    Txt=TabFEQ{PROP:Text} ; Undo = Txt
    OPEN(EditWn)
    0{PROP:Text}= 0{PROP:Text} & ' - ' & FeqName &' (FEQ: ' & TabFEQ &')'
    IF P[4] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
    ACCEPT
        CASE ACCEPTED()
        OF ?SetBtn  ; Ok2Set=1 ; BREAK
        OF ?UndoBtn ; Txt=Undo ; DISPLAY ; SELECT(?Txt)
        END
    END
    GETPOSITION(0,P[1],P[2],P[3],P[4])
    CLOSE(EditWn)
    IF Ok2Set THEN TabFEQ{PROP:Text}=Txt. ; DISPLAY
    RETURN
!================================================================================
ClaFeqName PROCEDURE(LONG F)!,STRING
c CSTRING(80)
  CODE
  IF ~F THEN RETURN 'Window'.
  c=ClaFieldNameRTL(F)
  RETURN CHOOSE(c<=' ','__Feq_'& F &'__',c)