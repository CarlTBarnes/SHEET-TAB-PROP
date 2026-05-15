!FlatTabTest by Carl Barnes - Copyright 2026 released under the MIT License
!---------------------------------------------------------------------------------------------------
! New in 12.0.14000 "FIX Drawing of TABs when the SHEET has the FLAT attribute."
! Prior to 14000 see Windows API "Tab Controls and Property Sheets" Styles TCS_FLATBUTTONS'
!
! This test program shows Normal and Flat sheets Side by Side
! This program could be repurposed to show other Tab Property effects Side-by-Side
! 
! Change History
! May 13 2026   New
! --------------------------------------------------------------------------------------------------

  PROGRAM
  Include('CBWndPreview.inc')   !https://github.com/CarlTBarnes/WindowPreview
  MAP
SheetTabAssistant   PROCEDURE()
ClaFeqName          PROCEDURE(LONG Feq),STRING
        MODULE('RTL')
ClaFieldNameRTL  PROCEDURE(LONG pFEQ),CSTRING,RAW,NAME('Cla$FIELDNAME'),DLL(dll_mode)
        END
  END

  CODE
  SheetTabAssistant()
    RETURN
!---------------------------------
SheetTabAssistant   PROCEDURE()

SHT CLASS                                                         !Build ReadProp STRING with data
Prop        PROCEDURE(LONG PropCode, BOOL TF, BOOL IsNegate=0)        !Set Sheet1{PropCode}=TF
ProValue     PROCEDURE(LONG PropCode, STRING ValueForProp)             !Set Sheet1{PropCode}=ValueForProp
ProIndex   PROCEDURE(LONG PropCode, LONG PropIndex, STRING ValueForProp)  !Set Sheet1{PropCode,PropIndex}=ValueForProp
    END

PropNoSheet  SHORT
PropWizard   SHORT
PropSpread   SHORT !TABs are evenly spaced on one line (PROP:SPREAD).
!PropFlat     SHORT !PROP:FLAT 05/12/26 added in C12.14000 "FIX Drawing of TABs when the SHEET has the FLAT attribute."
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
TabMaxCnt    EQUATE(9)
TabNdx       SHORT
TabLocation  SHORT(1)
TabLocPROP   LONG(PROP:Above)
TabLocWidth  SHORT(0)
TabMinWidth  SHORT(0)

TextReads    SHORT(1)
Scrolling    SHORT(1)
SheetStyle   SHORT(0)

!FYI: You can increase FONT(,Size) to be 12+ to more visually see the effects better -------------------
Window WINDOW('SHEET TAB - Flat Test - Property and Attribute Explorer'),AT(,,593,176),GRAY,SYSTEM, |
            ICON(ICON:Pick),FONT('Segoe UI',10),RESIZE
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
        CHECK('Flip Tabs Right -to- Left'),AT(9,123),USE(PropLayout)
        CHECK('No OS Theme / Visual Styles'),AT(9,132),USE(PropNoTheme)
        PROMPT('Tab Sheet Style:'),AT(9,144),USE(?Style:Prompt)
        LIST,AT(65,144,66,10),USE(SheetStyle),DROP(9),FROM('Default|#0|Black & White|#1|Colored|#2|S' & |
                'quared|#3|Boxed|#4')
        BUTTON('Text...'),AT(160,6,35,12),USE(?TextPickBtn),TIP('Popup to pick from a variety of Tab' & |
                ' Ear Label text<13,10>to change "Tab1, Tab2..." to Months, Colors or other text.')
        BUTTON('Icons...'),AT(200,6,35,12),USE(?IconPickBtn),TIP('Add Icons to Tab Ears')
        BUTTON('ReRun'),AT(302,6,30,12),USE(?ReRunBtn),TIP('Run another instance of Sheet Tab')
        PROMPT('Number of Tabs:'),AT(379,8),USE(?TabCount:Prompt)
        SPIN(@n2),AT(441,8,30,10),USE(TabCount),HVSCROLL,RANGE(1,9)
        GROUP,AT(540,4,49,156),USE(?GroupOfPropCHECK),FONT('Consolas')
            STRING('PROP:'),AT(555,6),USE(?Prop:Heading)
            CHECK('Above'),AT(543,14),USE(PropAbove2)
            CHECK('Below'),AT(543,23),USE(PropBelow2)
            CHECK('Left'),AT(543,32),USE(PropLeft2)
            CHECK('Right'),AT(543,40),USE(PropRight2)
            CHECK('UP  '),AT(543,52),USE(PropUp2)
            CHECK('Down'),AT(543,61),USE(PropDown2)
            CHECK('UpDown'),AT(543,70),USE(PropUpDown2)
            CHECK('HScroll'),AT(543,80),USE(PropHscroll2)
            CHECK('Join'),AT(543,89),USE(PropJoin2)
            CHECK('Broken'),AT(543,98),USE(PropBrokenTabs,, ?PropBrokenTabs:2)
            CHECK('Spread'),AT(543,110),USE(PropSpread,, ?PropSpread:2)
            CHECK('NoSheet'),AT(543,119),USE(PropNoSheet,, ?PropNoSheet:2)
            CHECK('Wizard'),AT(543,128),USE(PropWizard,, ?PropWizard:2)
            CHECK('Layout'),AT(543,137),USE(PropLayout,, ?PropLayout:2)
            CHECK('NoTheme'),AT(543,146),USE(PropNoTheme,, ?PropNoTheme:2)
        END
        GROUP,AT(156,28,179,128),USE(?Group:Sheet:A),BOXED,BEVEL(-1,1)
            SHEET,AT(160,32,170,120),USE(?Sheet:A)
                TAB('Tab1'),USE(?Tab1a)
                END
                TAB('Tab2'),USE(?Tab2a)
                END
                TAB('Tab3'),USE(?Tab3a)
                END
                TAB('Tab4'),USE(?Tab4a)
                END
                TAB('Tab5'),USE(?Tab5a)
                END
                TAB('Tab6'),USE(?Tab6a)
                END
                TAB('Tab7'),USE(?Tab7a)
                END
                TAB('Tab8'),USE(?Tab8a)
                END
                TAB('Tab9'),USE(?Tab9a),HIDE
                END
            END
        END
        GROUP,AT(346,28,179,128),USE(?Group:Sheet:B),BOXED,BEVEL(-1,1)
            SHEET,AT(350,32,170,120),USE(?Sheet:B)
                TAB('Tab1'),USE(?Tab1b)
                END
                TAB('Tab2'),USE(?Tab2b)
                END
                TAB('Tab3'),USE(?Tab3b)
                END
                TAB('Tab4'),USE(?Tab4b)
                END
                TAB('Tab5'),USE(?Tab5b)
                END
                TAB('Tab6'),USE(?Tab6b)
                END
                TAB('Tab7'),USE(?Tab7b)
                END
                TAB('Tab8'),USE(?Tab8b)
                END
                TAB('Tab9'),USE(?Tab9b),HIDE
                END
            END
        END
        STRING('Normal 3D Sheet'),AT(200,159),USE(?LabelSheet1),FONT(,14,,FONT:bold)
        STRING('Flat Sheet  (PROP:Flat)'),AT(376,159),USE(?LabelSheet1:2),FONT(,14,,FONT:bold)
    END

DOO CLASS                      !----- ROUTINE's replaced by DOO Class -----
PrepareWindow     PROCEDURE()  !After Open(Window) do some configure of window and controls
Prep_Manifest_ON  PROCEDURE()  !Config for Visual Styles if _Manifested_On_=>=On in Style project

TabLocationWidth  PROCEDURE()  !Take TabLocWidth input and set Sheet ABOVE( Width ) Prop:ABOVE{,2}=TabLocWidth or BELOW / LEFT / RIGHT
TipsOnControls    PROCEDURE()  !Set Tips on most controls using PROP:Tip, most PROPs have 2 controls with same Tip
TextOnTabPick     PROCEDURE()  !Popup to pick Tab('Text') so other than Tab1
IconOnTabPick     PROCEDURE()  !Popup to pick if want Icons on Tabs or not
    END
WndPrvCls   CBWndPreviewClass
    CODE
    OPEN(Window)
    WndPrvCls.Init()
    ?Sheet:B{PROP:Flat}=TRUE   !This is what we want to compare A to B
    DOO.PrepareWindow()
    DOO.TipsOnControls()

    ACCEPT
        CASE EVENT()
        OF EVENT:Rejected       ; DISPLAY(?) ; SELECT(?) ; CYCLE
        END
        CASE FIELD()
        OF ?TabLocWidth ; IF EVENT()=EVENT:NewSelection THEN POST(EVENT:Accepted,?).
        OF ?TabMinWidth ; IF EVENT()=EVENT:NewSelection THEN POST(EVENT:Accepted,?).
        OF ?TabCount
            CASE EVENT()
            OF EVENT:NewSelection ; POST(EVENT:Accepted,?TabCount)
            END        
        END
        CASE ACCEPTED()
        OF ?TabCount
               IF TabCount < 1 OR TabCount > TabMaxCnt THEN TabCount=TabMaxCnt.
               LOOP TabNdx=1 TO TabMaxCnt
                   (?Tab1a+TabNdx-1){PROP:Hide}=CHOOSE(TabNdx>TabCount)
                   (?Tab1b+TabNdx-1){PROP:Hide}=CHOOSE(TabNdx>TabCount)

!                   (?Tab1a+TabNdx-1){PROP:Text}='Ta'&TabNdx & CHOOSE(TabNdx>TabCount)
!                   (?Tab1b+TabNdx-1){PROP:Text}='Tb'&TabNdx & CHOOSE(TabNdx>TabCount)

               END        
        OF ?PropNoSheet OROF ?PropNoSheet:2 ; SHT.Prop(PROP:NoSheet,PropNoSheet)
        OF ?PropWizard  OROF ?PropWizard:2  ; SHT.Prop(PROP:Wizard, PropWizard)
        OF ?PropSpread  OROF ?PropSpread:2  ; SHT.Prop(PROP:Spread ,PropSpread)
        OF ?PropLayout  OROF ?PropLayout:2  ; SHT.Prop(PROP:Layout ,PropLayout)
        OF ?PropNoTheme OROF ?PropNoTheme:2 ; SHT.Prop(PROP:NoTheme,PropNoTheme)
                                              OMIT('!** Manifest ON **',_Manifested_On_)   !Project has: _Manifested_On_=>1
                                                    Message('This Project did not include a Manifest for Visual Styles.' & |
                                                            '|Checking the "No OS Theme" box will have no effect.' & |
                                                            '||To test this build and run the project with "_Style" in the name.|',0{PROP:Text})
                                              !** Manifest ON **         

        OF ?TabLocation ; TabLocPROP = CHOOSE(TabLocation,PROP:Above,PROP:Below,PROP:Left,PROP:Right)
                          SHT.Prop(TabLocPROP ,1  )
                          CLEAR(TabLocArray[]) ; TabLocArray[TabLocation]=1
                          IF TabLocWidth THEN DOO.TabLocationWidth().
                          DISPLAY
        OF ?TabLocWidth ; DOO.TabLocationWidth()
        OF ?TabMinWidth ; SHT.ProValue(PROP:MinWidth,TabMinWidth)

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
            SHT.ProValue(PROP:TabSheetStyle,SheetStyle)

        OF ?TextPickBtn   ; DOO.TextOnTabPick()
        OF ?IconPickBtn   ; DOO.IconOnTabPick()
        OF ?ReRunBtn      ; RUN(COMMAND('0')) ; CYCLE

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

    END !Accept
    RETURN

!----------------------------------------------------
DOO.PrepareWindow PROCEDURE()
  CODE
    0{PROP:text}=clip(0{PROP:text}) &' - Clarion ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    0{PROP:MinWidth}=0{PROP:Width}
    0{PROP:MaxWidth}=0{PROP:Width}
    0{PROP:MinHeight}=0{PROP:Height}
    0{PROP:MaxHeight}=0{PROP:Height} * 1.5

        COMPILE('!** Manifest ON **',_Manifested_On_)   !Project has: _Manifested_On_=>1
    DOO.Prep_Manifest_ON()
                 !** Manifest ON **

    PropBrokenTabs=?Sheet:A{Prop:BrokenTabs}  !RTL defaults to True
    PropLayout=?Sheet:A{PROP:Layout}          !May have set in Code ... unlikely
    PropNoTheme=?Sheet:A{PROP:NoTheme}        !May have set below


DOO.Prep_Manifest_ON PROCEDURE()
F LONG(0)
    CODE
    !Did not work: IF ~SYSTEM{PROP:ThemeActive} THEN EXIT.  !Was there a Manifest

!   ?SheetMain{PROP:NoTheme}=1
!    ?Sheet1{PROP:NoTheme}=1
    LOOP
        F=0{PROP:NExtField,F} ; IF ~F THEN BREAK.
        CASE F
!        OF ?TabRectBox ; CYCLE
        END
        CASE F{PROP:Type}
        OF CREATE:text ; CYCLE
        END
        F{PROP:Trn}=1
    END
    RETURN

!----------------------------
DOO.TabLocationWidth PROCEDURE()
  CODE
    SHT.ProIndex(TabLocPROP, 2, TabLocWidth)  ! ?Sheet1{(TabLocPROP),2}=TabLocWidth ; 
    SHT.Prop(TabLocPROP ,1  )
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

!---------------------------------------------------------
DOO.TextOnTabPick PROCEDURE()
PopNo   USHORT,AUTO
ShFEQ   LONG,AUTO  
ShNdx   USHORT,AUTO
TbFEQ   LONG,AUTO
TbNdx   USHORT,AUTO
    CODE
    PopNo = POPUP('Numbers (Tab 1, Tab 2...)|Nato Alphabet|Number Names|Month Names Long|Month Names Short' & |
                '|Colors|Controls|Paper|Character Sets|Christmas|A BB CCC DDDD ... MinWidth Test')
    IF ~PopNo THEN RETURN.
    LOOP ShNdx=1 TO 2
         ShFEQ=CHOOSE(ShNdx,?Sheet:A,?Sheet:B) 
         LOOP TbNdx=1 TO ShFEQ{PROP:NumTabs}
              TbFEQ=ShFEQ{PROP:Child,TbNdx}
              EXECUTE PopNo
                TbFEQ{PROP:Text}='Tab' & CHOOSE(TbNdx<=9,'&' & TbNdx, '1&' & TbNdx%10)
                TbFEQ{PROP:Text}=CHOOSE(TbNdx,'&Alfa','&Bravo','&Charlie','&Delta','&Echo','&Foxtrot','&Golf','&Hotel','&India','&Juliett','&Kilo','&Lima')
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
    END
    DISPLAY
    RETURN
!---------------------------------------------------------
DOO.IconOnTabPick PROCEDURE()
PopNo   USHORT,AUTO 
ShFEQ   LONG,AUTO  
ShNdx   USHORT,AUTO
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
    LOOP ShNdx=1 TO 2
         ShFEQ=CHOOSE(ShNdx,?Sheet:A,?Sheet:B)    
         LOOP TbNdx=1 TO ShFEQ{PROP:NumTabs}
              TbFEQ=ShFEQ{PROP:Child,TbNdx}
     !         IF TbFEQ{PROP:Hide} THEN CYCLE.
              CASE PopNo
              OF 1 ;  TbFEQ{PROP:Icon}=IconStr
              OF 2 ;  TbFEQ{PROP:Icon}=ICON:None
              OF 3 ;  TbFEQ{PROP:Icon}=''                !this shuts down the program. Sent to SV
              END
              IconEqt += IcoIncr     !Next  ICON:Save EQUATE('<0FFH,02H,02H,7FH>') ... 12= ICON:Clarion EQUATE('<0FFH,02H,0CH,7FH>')
         END
    END
    DISPLAY
    RETURN

!=============== Class Code ==============
SHT.Prop PROCEDURE(LONG PropCode, BOOL TF, BOOL IsNegate=0) !, BOOL NoDisplay=0)
    CODE
    IF IsNegate THEN TF=CHOOSE(~TF).
    ?Sheet:A{PropCode}=TF
    ?Sheet:B{PropCode}=TF
    DISPLAY
    RETURN 
SHT.ProValue PROCEDURE(LONG PropCode, STRING ValueForProp)  !Set Sheet1{PropCode}=ValueForProp
    CODE
    ?Sheet:A{PropCode} =ValueForProp
    ?Sheet:B{PropCode}=ValueForProp
    DISPLAY
    RETURN    
SHT.ProIndex PROCEDURE(LONG PropCode, LONG PropIndex, STRING ValueForProp)  !Set Sheet1{PropCode,PropIndex}=ValueForProp
    CODE
    ?Sheet:A{PropCode ,PropIndex}=ValueForProp
    ?Sheet:B{PropCode,PropIndex}=ValueForProp
    DISPLAY
    RETURN    

!================================================================================
ClaFeqName PROCEDURE(LONG F)!,STRING
c CSTRING(80)
  CODE
  IF ~F THEN RETURN 'Window'.
  c=ClaFieldNameRTL(F)
  RETURN CHOOSE(c<=' ','__Feq_'& F &'__',c)