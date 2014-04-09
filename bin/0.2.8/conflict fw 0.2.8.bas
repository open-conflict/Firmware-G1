

'###############################################################################
'################## Grundlegende Einstellungen zum Controller ##################
'######################## nach Befehlsliste V0.1.2 #############################
'###############################################################################


$regfile = "xm32a4def.dat"
$crystal = 32000000
$hwstack = 160
$swstack = 160
$framesize = 160

$loadersize = 4096

Config Osc = Enabled , 32mhzosc = Enabled , 32khzosc = Enabled                  'Takt einstellen
Osc_dfllctrl = &B00000000
Dfllrc32m_ctrl = 1
Config Sysclock = 32mhz , Prescalea = 1 , Prescalebc = 1_1

Config Eeprom = Mapped


'###############################################################################
'############################### Funktionen ####################################
'###############################################################################

Declare Function Convert_temp(byref Temperatur_wert As Byte) As String * 4
Declare Function Convert_4digit(byref Einword As Word) As String * 4
Declare Function Convert_3digit(byref Einbyte As Byte) As String * 3




'###############################################################################
'############################ Globale Variablen ################################
'###############################################################################



Dim Gui_version As Byte


Dim Screen As Byte                                                              'Aktuelle Displayanzeige
Dim Tempmask As Long
Dim Temp_ctr As Byte
Dim Zeilen_ctr As Byte
Tempmask = 1                                                                    'Setze die Maske zurück
Temp_ctr = 1                                                                    'setze den Zähler zurück
Dim Temp_ctr1 As Byte
Dim Temp_ctr2 As Byte
Dim Temp_ctr3 As Byte


Dim Ctr As Byte                                                                 'universielle zähl Variable

Dim Temperaturen(24) As Byte                                                    'Hier werden alle Temperaturen gespeichert
Dim Name_t(24) As String * 4                                                    'Namen der Temperaturen
Dim Te(8) As Byte                                                               'Die Acht Ersatztemperaturen


Dim Gueltigkeit As Byte                                                         '8 Bit die darüber entscheiden ob Tachoswerte und AIDA Werte noch gültig sind


Dim Led_status As Byte                                                          'Status der LED Kanäle
Dim Led_help As Byte                                                            'Wird für die Automatischen Frabverläufe gebraucht
Dim Led(3) As Byte                                                              'Die 3 PWM Werte für die LED's

Dim Lcd_contrast As Byte                                                        'Der Kontrast fürs Display 0-63
Dim Tempvar_1 As Byte                                                           'Werden zur Kontrasteinstellung
Dim Tempvar_2 As Byte                                                           'benötigt

Dim Show(3) As Byte                                                             '24 Bit die entscheiden Welche Temperaturen angezeigt werden sollen
Dim Show_dfm As Byte
Dim Show_aida As Byte                                                           '=1 wenn der DFM Screen angezeigt werden soll

Dim Temp_byte As Byte
Dim Temp_byte2 As Byte
Dim Temp_word As Word
Dim Temp_single As Single
Dim Temp_long As Long
Dim Temp_long2 As Long
Dim Temp_long3 As Long
Dim Temp_4$ As String * 4
Dim Temp_interrupt As Byte                                                      'darf nur in Interrupts benutzt werden

Dim Timectr As Byte                                                             'Wird benötigt um Timeticks zu berechnen
Dim Timeticks As Byte                                                           'Die Bits werden Zeitgesteuert aktiviert und dienen als Trigger
Dim Timectr2 As Byte
'Timeticks.0 wird alle 100ms gesetzt
'Timeticks.1 alle 250ms
'Timeticks.2 alle 500ms
'Timeticks.3 jede Sekunde
'Timeticks.4 alle 10 sekunden



Dim Pumpe_spannung As Byte                                                      'Spannung der Pumpe in Volt x10 (z.B. 085 = 8,5V)
Dim Pumpe_anlauf As Byte
Dim Pumpe_anlauf_t As Byte
Dim Pumpe_status As Byte

Dim F_aus_auto As Byte                                                          'Hier wird aus und auto modus für alle 3 Fan Kanäle gespeichert
'                        Bitweise
 ' 7  6     5       4         3      2        1       0
 ' 0  0  F1_aus  F1_auto  F2_aus  F2_auto  F3_aus  F3_auto


Dim F1_status As Byte                                                           'Lüfter Status
Dim F1_min As Byte                                                              'Minimaler PWM Wert
Dim F1_anlauf As Byte                                                           'Konstante für Anlaufzeit
Dim F1_anlauf_t As Byte                                                         'verbleibende Anlaufzeit
Dim F1_anlauf_schwelle As Byte                                                  'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
F1_auto Alias F_aus_auto.4                                                      '1 wenn automatische Regelung
F1_aus Alias F_aus_auto.5                                                       '1 wenn Lüfter aus gehen darf
Dim Manuell1 As Byte                                                            'Manueller Wert
Dim Min1_t(24) As Byte                                                          '24 Minimum Temperaturen
Dim Max1_t(24) As Byte                                                          '24 Maximum Temperaturen
Dim Kuehlung1 As Byte

Dim F2_status As Byte                                                           'Lüfter Status
Dim F2_min As Byte                                                              'Minimaler PWM Wert
Dim F2_anlauf As Byte                                                           'Konstante für Anlaufzeit
Dim F2_anlauf_t As Byte                                                         'verbleibende Anlaufzeit
Dim F2_anlauf_schwelle As Byte                                                  'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
F2_auto Alias F_aus_auto.2                                                      '1 wenn automatische Regelung
F2_aus Alias F_aus_auto.3                                                       '1 wenn Lüfter aus gehen darf
Dim Manuell2 As Byte                                                            'Manueller Wert
Dim Min2_t(24) As Byte                                                          '24 Minimum Temperaturen
Dim Max2_t(24) As Byte                                                          '24 Maximum Temperaturen
Dim Kuehlung2 As Byte


Dim F3_status As Byte                                                           'Lüfter Status
Dim F3_min As Byte                                                              'Minimaler PWM Wert
Dim F3_anlauf As Byte                                                           'Konstante für Anlaufzeit
Dim F3_anlauf_t As Byte                                                         'verbleibende Anlaufzeit
Dim F3_anlauf_schwelle As Byte                                                  'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
F3_auto Alias F_aus_auto.0                                                      '1 wenn automatische Regelung
F3_aus Alias F_aus_auto.1                                                       '1 wenn Lüfter aus gehen darf
Dim Manuell3 As Byte                                                            'Manueller Wert
Dim Min3_t(24) As Byte                                                          '24 Minimum Temperaturen
Dim Max3_t(24) As Byte                                                          '24 Maximum Temperaturen
Dim Kuehlung3 As Byte


Dim Alarm_setup As Byte                                                         'Einstellungs Byte für die Alarm Einstellungen
Dim Alarm_status As Byte                                                        'Aktueller Status
Dim Alarm_enable As Bit                                                         'Global Alarm aktiviert

' X X X F1 F2 F3 T D


Dim Dfm_imp As Word                                                             'Impulse pro Liter für DFM
Dim Dfm_ctr As Byte
Dim Dfm_average(10) As Byte

For Temp_byte = 1 To 10
    Dfm_average(temp_byte) = 50
Next

Dim Cv_dfm As Word                                                              'Hier steht der aktuelle DFM Zählwert
Dim Cv_dfm_l As Byte At Cv_dfm Overlay
Dim Cv_dfm_h As Byte At Cv_dfm + 1 Overlay

Dim Cv_dfm_alt As Word
Dim Cv_dfm_alt_l As Byte At Cv_dfm_alt Overlay
Dim Cv_dfm_alt_h As Byte At Cv_dfm_alt + 1 Overlay


Dim Cv_fan1 As Word                                                             'Hier steht der aktuelle Fan1 Zählwert
Dim Cv_fan1_l As Byte At Cv_fan1 Overlay
Dim Cv_fan1_h As Byte At Cv_fan1 + 1 Overlay

Dim Cv_fan1_alt As Word
Dim Cv_fan1_alt_l As Byte At Cv_fan1_alt Overlay
Dim Cv_fan1_alt_h As Byte At Cv_fan1_alt + 1 Overlay


Dim Cv_fan2 As Word                                                             'Hier steht der aktuelle Fan2 Zählwert
Dim Cv_fan2_l As Byte At Cv_fan2 Overlay
Dim Cv_fan2_h As Byte At Cv_fan2 + 1 Overlay

Dim Cv_fan2_alt As Word
Dim Cv_fan2_alt_l As Byte At Cv_fan2_alt Overlay
Dim Cv_fan2_alt_h As Byte At Cv_fan2_alt + 1 Overlay


Dim Cv_fan3 As Word                                                             'Hier steht der aktuelle Fan3 Zählwert
Dim Cv_fan3_l As Byte At Cv_fan3 Overlay
Dim Cv_fan3_h As Byte At Cv_fan3 + 1 Overlay

Dim Cv_fan3_alt As Word
Dim Cv_fan3_alt_l As Byte At Cv_fan3_alt Overlay
Dim Cv_fan3_alt_h As Byte At Cv_fan3_alt + 1 Overlay

Dim Durchfluss As Word                                                          'Liter Pro Stunde
Dim Durchfluss_alarm As Word

Dim Tacho1 As Word
Dim Tacho2 As Word
Dim Tacho3 As Word

Dim Cpu_takt_$ As String * 4
Cpu_takt_$ = "----"
Dim Cpu_auslastung_$ As String * 3
Cpu_auslastung_$ = "---"
Dim Gpu_auslastung_$ As String * 3
Gpu_auslastung_$ = "---"
Dim Ram_auslastung_$ As String * 3
Ram_auslastung_$ = "---"

Dim Taster_links_ctr As Byte
Dim Taster_rechts_ctr As Byte

Dim 1_wireanzahl As Word
Dim Adr_t(64) As Byte                                                           'Die Adressen für die DS18S20 Temperatursensoen

Dim Com_r_ctr As Byte
Dim Com_r As String * 253                                                       'Hier wird erstmal alles gesammelt was über den Comport kommt

Dim Datenwort(51) As String * 4

'Umbenennung der PWM Register

Fan1 Alias Tce0_ccbbuf
Fan2 Alias Tce0_cccbuf
Fan3 Alias Tce0_ccdbuf

Led_b Alias Tcd0_ccabuf
Led_g Alias Tcd0_ccbbuf
Led_r Alias Tcd0_cccbuf

Pumpe Alias Tcd0_ccdbuf
Led_back Alias Tce0_ccabuf



'###############################################################################
'##################### Aus und Eingänge Einstellen #############################
'###############################################################################

Porta_dir = 0
Portb_dir = &B00001100
Portc_dir = &B10111100
Portd_dir = &B00001111
Porte_dir = &B00001111
Portr_dir = &B00000001

Portb_pin0ctrl = &B00011000                                                     'Pullup Taster
Portd_pin4ctrl = &B00011000                                                     'Pullup Taster
Taster_rechts Alias Pinb.0
Taster_links Alias Pind.4

                                                            'Falling edge sense für Zeitmessung
Portb_pin1ctrl = &B00000010                                                     'DFM
Portd_pin5ctrl = &B00000010                                                     'Tacho 1
Portd_pin6ctrl = &B00000010                                                     'Tacho 2
Portd_pin7ctrl = &B00000010                                                     'Tacho 3


Summer Alias Portr.0
Ds18s20 Alias Portr.1





'###############################################################################
'###################### Variablen Startwerte zuweisen ##########################
'######################        und EEPROM lesen       ##########################
'###############################################################################

$eeprom
$eepromhex

 Data 160 , 160 , 160 , 160 , 160 , 160 , 160 , 160                             '8 Ersatztemperaturen

 Data 255 , 255 , 255                                                           '24 Bit die entscheiden Welche Temperaturen angezeigt werden sollen
 Data 1                                                                         'Show_DFM

 Data 120                                                                       'Spannung der Pumpe in Volt x10 (z.B. 085 = 8,5V)


 Data 100 , 4 , 20 , 255                                                        'Min, Anlaufzeit, % Schwelle, Manuell
 Data 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250
 Data 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255

 Data 100 , 4 , 20 , 255
 Data 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250
 Data 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255

 Data 100 , 4 , 20 , 255
 Data 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250 , 250
 Data 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255 , 255


 Data 0                                                                         'Aus und Auto für die Lüfter

 '                        Bitweise
 ' 7  6     5       4         3      2        1       0
 ' 0  0  F1_aus  F1_auto  F2_aus  F2_auto  F3_aus  F3_auto


 Data 15 , 200                                                                  'Contrast und Backlight

 Data "Te 1" , "Te 2" , "Te 3" , "Te 4" , "Te 5" , "Te 6" , "Te 7" , "Te 8" , "Te 9" , "Te10" , "Te11" , "Te12"
 Data "Te13" , "Te14" , "Te15" , "Te16" , "Te17" , "Te18" , "Te19" , "Te20" , "Te21" , "Te22" , "Te23" , "Te24"

 Data 1200%                                                                     'IMP/Liter

 Data 2                                                                         'LED Status
 Data 255 , 255 , 255                                                           'Led Werte

 Data &B00000000                                                                'Alarm Setup

 Data 20%                                                                       'Durchfluss_alarm

 Data 30                                                                        'Anlaufzeit der Pumpe

$data


Dim Te_e(8) As Eram Byte

For Ctr = 1 To 8
    Te(ctr) = Te_e(ctr)
    Temperaturen(ctr + 16) = Te(ctr)                                            'Die Acht Ersatztemperaturen
Next


Dim Show_e(3) As Eram Byte
Dim Show_dfm_e As Eram Byte
For Ctr = 1 To 3
    Show(ctr) = Show_e(ctr)                                                     '24 Bit die entscheiden Welche Temperaturen angezeigt werden sollen
Next
Show_dfm = Show_dfm_e

Dim Pumpe_spannung_e As Eram Byte
Pumpe_spannung = Pumpe_spannung_e                                               'Spannung der Pumpe in Volt x10 (z.B. 085 = 8,5V)
Pumpe_anlauf = 30                                                               '15 Sekunden anlaufzeit für die Pumpe

Dim F1_min_e As Eram Byte                                                       'Minimaler PWM Wert
Dim F1_anlauf_e As Eram Byte                                                    'Konstante für Anlaufzeit
Dim F1_anlauf_schwelle_e As Eram Byte                                           'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
Dim Manuell1_e As Eram Byte                                                     'Manueller Wert
Dim Min1_t_e(24) As Eram Byte                                                   '24 Minimum Temperaturen
Dim Max1_t_e(24) As Eram Byte

F1_status = 1                                                                   'Lüfter Status
F1_min = F1_min_e                                                               'Minimaler PWM Wert
F1_anlauf = F1_anlauf_e                                                         'Konstante für Anlaufzeit
F1_anlauf_t = F1_anlauf
F1_anlauf_schwelle = F1_anlauf_schwelle_e                                       'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
Manuell1 = Manuell1_e                                                           'Manueller Wert
For Ctr = 1 To 24
    Min1_t(ctr) = Min1_t_e(ctr)                                                 '24 Minimum Temperaturen
    Max1_t(ctr) = Max1_t_e(ctr)                                                 '24 Maximum Temperaturen
Next




Dim F2_min_e As Eram Byte                                                       'Minimaler PWM Wert
Dim F2_anlauf_e As Eram Byte                                                    'Konstante für Anlaufzeit
Dim F2_anlauf_schwelle_e As Eram Byte                                           'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
Dim Manuell2_e As Eram Byte                                                     'Manueller Wert
Dim Min2_t_e(24) As Eram Byte                                                   '24 Minimum Temperaturen
Dim Max2_t_e(24) As Eram Byte

F2_status = 1                                                                   'Lüfter Status
F2_min = F2_min_e                                                               'Minimaler PWM Wert
F2_anlauf = F2_anlauf_e                                                         'Konstante für Anlaufzeit
F2_anlauf_t = F2_anlauf
F2_anlauf_schwelle = F2_anlauf_schwelle_e                                       'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
Manuell2 = Manuell2_e                                                           'Manueller Wert
For Ctr = 1 To 24
    Min2_t(ctr) = Min2_t_e(ctr)                                                 '24 Minimum Temperaturen
    Max2_t(ctr) = Max2_t_e(ctr)                                                 '24 Maximum Temperaturen
Next


Dim F3_min_e As Eram Byte                                                       'Minimaler PWM Wert
Dim F3_anlauf_e As Eram Byte                                                    'Konstante für Anlaufzeit
Dim F3_anlauf_schwelle_e As Eram Byte                                           'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
Dim Manuell3_e As Eram Byte                                                     'Manueller Wert
Dim Min3_t_e(24) As Eram Byte                                                   '24 Minimum Temperaturen
Dim Max3_t_e(24) As Eram Byte

F3_status = 1                                                                   'Lüfter Status
F3_min = F3_min_e                                                               'Minimaler PWM Wert
F3_anlauf = F3_anlauf_e                                                         'Konstante für Anlaufzeit
F3_anlauf_t = F3_anlauf
F3_anlauf_schwelle = F3_anlauf_schwelle_e                                       'Bei wie viel % Kühlbedarf der Lüfter wieder an geht
Manuell3 = Manuell3_e                                                           'Manueller Wert
For Ctr = 1 To 24
    Min3_t(ctr) = Min3_t_e(ctr)                                                 '24 Minimum Temperaturen
    Max3_t(ctr) = Max3_t_e(ctr)                                                 '24 Maximum Temperaturen
Next

Dim F_aus_auto_e As Eram Byte                                                   'Die Aus und Auto Bits
F_aus_auto = F_aus_auto_e


Dim Lcd_contrast_e As Eram Byte
Dim Led_back_e As Eram Byte

Lcd_contrast = Lcd_contrast_e                                                   'Der Kontrast fürs Display 0-63
Temp_byte = Led_back_e
Led_back = Temp_byte

Dim Name_t_e(24) As Eram String * 4
For Ctr = 1 To 24
    Name_t(ctr) = Name_t_e(ctr)
Next

Dim Dfm_imp_e As Eram Word
Dfm_imp = Dfm_imp_e                                                             'Impulse pro Liter für DFM


Dim Led_status_e As Eram Byte
Led_status = Led_status_e                                                       'Status der LED Kanäle
Dim Led_e(3) As Eram Byte
For Ctr = 1 To 3
    Led(ctr) = Led_e(ctr)
Next                                                                            'LED Werte


Dim Alarm_setup_e As Eram Byte                                                  'Einstellungs Byte für die Alarm Einstellungen
Alarm_setup = Alarm_setup_e

Dim Durchfluss_alarm_e As Eram Word                                             'Alarmschwelle für den Durchfluss
Durchfluss_alarm = Durchfluss_alarm_e


Dim Pumpe_anlauf_e As Eram Byte                                                 'Konstante für Anlaufzeit
Pumpe_anlauf = Pumpe_anlauf_e                                                   'Konstante für Anlaufzeit
Pumpe_anlauf_t = Pumpe_anlauf



'###############################################################################
'############################### LCD Konfigurieren #############################
'###############################################################################


Config Lcdpin = Pin , Db4 = Portc.2 , Db5 = Portc.3 , Db6 = Portc.4 , Db7 = Portc.5 , E = Portb.2 , Rs = Portb.3
Config Lcd = 16 * 3 , Chipset = Dogm163v3                                       '16*3 type LCD display
Gosub Lcd_kontrastset
Cursor Off
Cls                                                                             'Sauber machen


'###############################################################################
'########################## Com Port Konfigurieren #############################
'###############################################################################

Config Com2 = 57600 , Mode = Asynchroneous , Parity = None , Stopbits = 1 , Databits = 8
Open "com2:" For Binary As #1
On Usartc1_rxc Recom
Enable Usartc1_rxc , Lo

Const Crlf = "{013}{010}"                                                       'Constante mit Carrige Return und Line Feed

'TXD = PORTC.7
'RXD = PORTC.6
'Das ist USARTC1



'###############################################################################
'################ TC für Dual Slope PWM ca. 32KHz einstellen ###################
'###############################################################################

Tce0_ctrlb = &B11110101
Tce0_per = 255
Tce0_ctrla = &B00000010

Tcd0_ctrlb = &B11110101
Tcd0_per = 255
Tcd0_ctrla = &B00000010


Gosub Led_init                                                                  'Startwerte für LED's setzen


'###############################################################################
'################### TCC0 zur Periodendauermessung einstellen ##################
'###############################################################################

Evsys_ch0mux = &B01011001                                                       'PORTB.1 DFM
Evsys_ch1mux = &B01101101                                                       'PORTD.5 Tacho 1
Evsys_ch2mux = &B01101110                                                       'PORTD.6 Tacho 2
Evsys_ch3mux = &B01101111                                                       'PORTD.7 Tacho 3
' Zuerst mit dem Event-System die Eingänge auf die Compare Channels routen
         '0101 0 n PORTA_PINn PORTA Pin n (n= 0, 1, 2 ... or 7)
         '0101 1 n PORTB_PINn PORTB Pin n (n= 0, 1, 2 ... or 7)
         '0110 0 n PORTC_PINn PORTC Pin n (n= 0, 1, 2 ... or 7)
         '0110 1 n PORTD_PINn PORTD Pin n (n= 0, 1, 2 ... or 7)
         '0111 0 n PORTE_PINn PORTE Pin n (n= 0, 1, 2 ... or 7)
         '0111 1 n PORTF_PINn PORTF Pin n (n= 0, 1, 2 ... or 7)



Tcc0_ctrld = &B00101000                                                         'Event Channels 0-3 für Capture ausgewählt
Tcc0_ctrlb = &B11110000                                                         'Compare Channel A bis D aktiviert
Tcc0_per = &HFFFF

Tcc0_ctrla = &B00000110                                                         'Prescaler 256

On Tcc0_cca C_dfm Nosave
On Tcc0_ccb C_fan1 Nosave
On Tcc0_ccc C_fan2 Nosave
On Tcc0_ccd C_fan3 Nosave


Enable Tcc0_cca , Lo
Enable Tcc0_ccb , Lo
Enable Tcc0_ccc , Lo
Enable Tcc0_ccd , Lo



'###############################################################################
'############################ AD-Wandler Konfigurieren #########################
'###############################################################################


'First we read the Calibration bytes form Signature Row (to get the real 12-Bit)
Temp_byte = Readsig(&H20)
Temp_byte2 = Readsig(&H21)

'Write factory calibration values to calibration register
Adca_call = Temp_byte
Adca_calh = Temp_byte2


Config Adca = Single , Convmode = Unsigned , Resolution = 12bit , _
Dma = Off , Reference = Int1v , Event_mode = None , Prescaler = 32 , _
Ch0_gain = 1 , Ch0_inp = Single_ended , Mux0 = &B00000000 , _
Bandgap = Enabled , Tempref = Enabled


'###############################################################################
'############################ 1-Wire Konfigurieren #############################
'###############################################################################



'PORTF.6 ist der DS18S20
Config 1wire = Ds18s20                                                          'PORTR.1


Const Read_rom = &H33                                                           'Liest die Sensor ID (nur bei 1 Sensor)
Const Match_rom = &H55                                                          'Wählt einen bestimmten Sensor aus (nachfolgend muss Sensor ID gesendet werden)
Const Skip_rom = &HCC                                                           'Überspringt Sensorwahl (bei einem Sensor, oder bei einem Befehl an alle angeschlossenen Sensoren)
Const Convertt = &H44                                                           'Weißt Temperatursensor an die Temperatur zu messen
Const Read_ram = &HBE                                                           'Liest den Ramspeicher
Const Write_ram = &H4E                                                          'Schreibt den Ramspeicher
Const Copy_ram = &H48                                                           'Speichert T Register in den EEPROM
Const Recall_ee = &HB8                                                          'Lädt T Register aus dem EEPROM in den RAM
Const Read_power = &HB4                                                         'Liest Die Spannungsversorgungsart



1wreset

1_wireanzahl = 1wirecount()                                                     'Nachschauen wie viele Sensoren angeschlossen sind


'Jetzt die Adressen auslesen
If 1_wireanzahl > 0 Then Adr_t(1) = 1wsearchfirst()                             'wir haben mindestens einen Sensor

If 1_wireanzahl > 1 Then                                                        'wir haben mehr als einen Sensor

   Temp_byte2 = 1_wireanzahl - 1

   For Temp_byte = 1 To Temp_byte2
       Temp_word = 8 * Temp_byte

       Adr_t(temp_word + 1) = 1wsearchnext()

   Next


End If

If 1_wireanzahl < 8 Then                                                        'wenn nicht voll bestückt

    Ctr = 8
    While Ctr > 1_wireanzahl

          Temperaturen(ctr + 8) = 220                                           'Setze die nicht benutzten Temperaturen auf aus

          Decr Ctr
    Wend

End If



1wreset
1wwrite Skip_rom
1wwrite Convertt




'###############################################################################
'######################## TCC1 für GP einstellen ###############################
'###############################################################################

Tcc1_per = &HFFFF                                                               'Nutze alle 16 Bit
Tcc1_ctrla = &B00000111                                                         'Prescaler = 1024





'###############################################################################
'############################## Interrupts aktivieren ##########################
'###############################################################################


Config Priority = Static , Vector = Application , Lo = Enabled
Enable Interrupts
Alarm_enable = 1


'###############################################################################
'################################# Main Do-Loop ################################
'###############################################################################

Do


  Debounce Taster_rechts , 0 , Next_screen , Sub
  Debounce Taster_links , 0 , Alarm_toggle , Sub


  If Tcc1_cnt > 3125 Then                                                       'alle 100ms
     Tcc1_cnt = 0

     'If Taster_links = 0 Then                                                   'Linker taster gedrückt
'        Incr Taster_links_ctr                                                   'Zähle variable eins hoch
'     Else
'        Taster_links_ctr = 0                                                    'Setze Variable gleich 0
'     End If



     Incr Timectr

     Set Timeticks.0                                                            'Alle 100 ms

     If Timectr.0 = 0 Then Set Timeticks.1                                      'Wenn der Zähler durch 2 Teilbar ist, sind 200ms rum

     If Timectr = 5 Or Timectr = 10 Then                                        'Nach 500ms

        Gosub Display_akt                                                       'aktualisiere Display
        Gosub Get_analog_temperatures                                           'Analoge Sensoren abfragen
        If 1_wireanzahl > 0 Then Gosub Get_digital_temperatures                 'Digitale Sensoren abfragen
        Gosub Count_startup_time                                                'Startzeiten herunterzählen

     End If


     If Timectr = 10 Then                                                       'Nach 1s
        Timectr = 0                                                             'Den Zähler zurücksetzen
        Gosub Check_gueltigkeit
        Gosub Check_alarm
        Gosub Send_refresh

     End If


     Incr Timectr2


     If Timectr2 = 50 Then Gosub Check_aida                                     'Wenn 5s rum sind



     If Timectr2 = 100 Then                                                     'Zehn Sekunden rum

        If Alarm_enable = 0 Then
           Set Summer
           Waitms 10
           Reset Summer
        End If
        Gosub Check_aida
        Gosub Next_screen
     End If



     'Timeticks.0 wird alle 100ms gesetzt
     'Timeticks.1 alle 200ms




  End If




'###############################################################################
'#############################    LED's berechnen    ###########################
'###############################################################################


'00        Manueller Modus, LED1-3 werden verwendet
'01        Color Change 1
'10        Color Change 2
'11        Color Change 3


  Select Case Led_status

       'Case 0

'            Led_r = Led(1)
'            Led_g = Led(2)
'            Led_b = Led(3)


       Case 1

            If Timeticks.0 = 1 Then                                             'Alle 100ms

               Reset Timeticks.0

               Select Case Led_help

                      Case 0                                                    '1. Farbe hoch
                           Temp_byte = Led_r
                           Incr Temp_byte
                           Led_r = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 1                                                    '2. Farbe hoch
                           Temp_byte = Led_g
                           Incr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 2                                                    '3. Farbe hoch 1. Runter
                           Temp_byte = Led_b
                           Incr Temp_byte
                           Led_b = Temp_byte
                           Temp_byte = Led_r
                           Decr Temp_byte
                           Led_r = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 3                                                    '2. Farbe runter
                           Temp_byte = Led_g
                           Decr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 4                                                    '3. Farbe runter
                           Temp_byte = Led_b
                           Decr Temp_byte
                           Led_b = Temp_byte
                           If Temp_byte = 0 Then Led_help = 0

               End Select



            End If


       Case 2

            If Timeticks.0 = 1 Then                                             'Alle 100ms

               Reset Timeticks.0

               Select Case Led_help

                      Case 0                                                    '001
                           Temp_byte = Led_b
                           Incr Temp_byte
                           Led_b = Temp_byte
                           Temp_byte = Led_g
                           Decr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 1                                                    '011
                           Temp_byte = Led_g
                           Incr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 2                                                    '111
                           Temp_byte = Led_r
                           Incr Temp_byte
                           Led_r = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 3                                                    '101
                           Temp_byte = Led_g
                           Decr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 4                                                    '100
                           Temp_byte = Led_b
                           Decr Temp_byte
                           Led_b = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 5                                                    '110
                           Temp_byte = Led_g
                           Incr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 6                                                    '010
                           Temp_byte = Led_r
                           Decr Temp_byte
                           Led_r = Temp_byte
                           If Temp_byte = 0 Then Led_help = 0



               End Select



            End If


       Case 3

            If Timeticks.1 = 1 Then                                             'Alle 200ms

               Reset Timeticks.1

               Select Case Led_help

                      Case 0                                                    '001
                           Temp_byte = Led_b
                           Incr Temp_byte
                           Led_b = Temp_byte
                           Temp_byte = Led_g
                           Decr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 1                                                    '011
                           Temp_byte = Led_g
                           Incr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 2                                                    '111
                           Temp_byte = Led_r
                           Incr Temp_byte
                           Led_r = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 3                                                    '101
                           Temp_byte = Led_g
                           Decr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 4                                                    '100
                           Temp_byte = Led_b
                           Decr Temp_byte
                           Led_b = Temp_byte
                           If Temp_byte = 0 Then Incr Led_help

                      Case 5                                                    '110
                           Temp_byte = Led_g
                           Incr Temp_byte
                           Led_g = Temp_byte
                           If Temp_byte = 255 Then Incr Led_help

                      Case 6                                                    '010
                           Temp_byte = Led_r
                           Decr Temp_byte
                           Led_r = Temp_byte
                           If Temp_byte = 0 Then Led_help = 0



               End Select



            End If


  End Select




'###############################################################################
'####################### PWM-Wert für Pumpe berechnen ##########################
'###############################################################################


  Select Case Pumpe_status

     Case 0:                                                                    'Pumpe ist aus

        Pumpe = 0
        If Pumpe_spannung > 0 Then
           Incr Pumpe_status
           Pumpe_anlauf_t = Pumpe_anlauf                                        'Pumpe gibt x s lang gas
        End If

     Case 1:                                                                    'Pumpe läuft an

        Pumpe = 255

     Case 2 :                                                                   'Pumpe Läuft Normal


        Select Case Pumpe_spannung                                              'Wenn Spannung größer 12V dann

           Case Is > 120 :
                Pumpe = 255                                                     'Vollgas

           Case 1 To 120:                                                       'Wenn Spannung 0,1-12V
                Temp_single = Pumpe_spannung
                Temp_single = Temp_single * 1.7
                Temp_byte = Temp_single
                Pumpe = Temp_byte

           Case 0
                Pumpe_status = 0

        End Select

  End Select



'###############################################################################
'################################ Berechnung Fan1 ##############################
'###############################################################################

  Reset Alarm_status.1                                                          'Setze den Temperaturalarm zurück
  'Dieser wird wieder gesetzt sollte ein Temperaturalarm (weiterhin) bestehen!


  If F1_auto = 1 Then                                                           'Wenn Lüfter automatisch geregelt werden soll

      Kuehlung1 = 0

      For Ctr = 1 To 24                                                         'berechne für jede Temperatur den Kühlungsbedarf

         If Temperaturen(ctr) >= Min1_t(ctr) And Temperaturen(ctr) <= Max1_t(ctr) Then       'Temperatur ist zwischen Min und Max

            Temp_byte = Max1_t(ctr) - Min1_t(ctr)
            Temp_byte = 100 / Temp_byte

            Temp_byte2 = Temperaturen(ctr) - Min1_t(ctr)
            Temp_byte2 = Temp_byte2 * Temp_byte

            If Temp_byte2 > Kuehlung1 Then Kuehlung1 = Temp_byte2               'Speichere den höchsten Kühlungsbedarf



         End If


         If Temperaturen(ctr) > Max1_t(ctr) Then                                'Temperatur ist Größer als Max

                  ' X X X F1 F2 F3 T D
                  Set Alarm_status.1

                  Kuehlung1 = 100


         End If

      Next




      Select Case F1_status

         Case 0:                                                                'Lüfter ist aus

            Fan1 = 0                                                            'Lüfter aus
            If F1_aus = 1 Then                                                  'Lüfter darf aus
               If Kuehlung1 > F1_anlauf_schwelle Then
                  Incr F1_status                                                'Lüfter soll anlaufen
                  F1_anlauf_t = F1_anlauf                                       'Anlaufzeit zurücksetzen
               End If
            Else                                                                'Lüfter sollte aber laufen
               Incr F1_status                                                   'Lüfter soll anlaufen
               F1_anlauf_t = F1_anlauf                                          'Anlaufzeit zurücksetzen
            End If

         Case 1:                                                                'Lüfter läuft an

            Fan1 = 255                                                          'Vollgas


         Case 2:                                                                'Lüfter läuft Normal

            If F1_aus = 1 And Kuehlung1 = 0 Then F1_status = 0                  'Wenn keine Kühlung benötigt und der Lüfter aus gehen darf
            Temp_byte2 = 255 - F1_min
            Temp_single = Temp_byte2 / 100
            Temp_byte2 = Temp_single * Kuehlung1
            Temp_byte2 = Temp_byte2 + F1_min
            Fan1 = Temp_byte2







      End Select



  Else                                                                          'Wenn Manuelle Einstellung

      Select Case F1_status

         Case 0:                                                                'Lüfter ist aus

            Fan1 = 0                                                            'Lüfter aus
            If Manuell1 > 0 Then
               Incr F1_status                                                   'Lüfter soll anlaufen
               F1_anlauf_t = F1_anlauf                                          'Anlaufzeit zurücksetzen
            End If


         Case 1:                                                                'Lüfter läuft an

            Fan1 = 255                                                          'Vollgas


         Case 2:                                                                'Lüfter läuft Normal

            If Manuell1 = 0 Then                                                'Wenn 0 eingestellt, soll der Lüfter auch in den Auszustand
               F1_status = 0                                                    'Lüfter soll aus
            Else
               Fan1 = Manuell1
            End If




      End Select


  End If




'###############################################################################
'################################ Berechnung Fan2 ##############################
'###############################################################################


  If F2_auto = 1 Then                                                           'Wenn Lüfter automatisch geregelt werden soll

      Kuehlung2 = 0

      For Ctr = 1 To 24                                                         'berechne für jede Temperatur den Kühlungsbedarf

         If Temperaturen(ctr) >= Min2_t(ctr) And Temperaturen(ctr) <= Max2_t(ctr) Then       'Temperatur ist zwischen Min und Max

            Temp_byte = Max2_t(ctr) - Min2_t(ctr)
            Temp_byte = 100 / Temp_byte

            Temp_byte2 = Temperaturen(ctr) - Min2_t(ctr)
            Temp_byte2 = Temp_byte2 * Temp_byte

            If Temp_byte2 > Kuehlung2 Then Kuehlung2 = Temp_byte2               'Speichere den höchsten Kühlungsbedarf



         End If


         If Temperaturen(ctr) > Max2_t(ctr) Then                                'Temperatur ist Größer als Max

                  ' X X X F1 F2 F3 T D
                  Set Alarm_status.1

                  Kuehlung2 = 100


         End If

      Next




      Select Case F2_status

         Case 0:                                                                'Lüfter ist aus

            Fan2 = 0                                                            'Lüfter aus
            If F2_aus = 1 Then                                                  'Lüfter darf aus
               If Kuehlung2 > F2_anlauf_schwelle Then
                  Incr F2_status                                                'Lüfter soll anlaufen
                  F2_anlauf_t = F2_anlauf                                       'Anlaufzeit zurücksetzen
               End If
            Else                                                                'Lüfter sollte aber laufen
               Incr F2_status                                                   'Lüfter soll anlaufen
               F2_anlauf_t = F2_anlauf                                          'Anlaufzeit zurücksetzen
            End If


         Case 1:                                                                'Lüfter läuft an

            Fan2 = 255                                                          'Vollgas


         Case 2:                                                                'Lüfter läuft Normal

            If F2_aus = 1 And Kuehlung2 = 0 Then F2_status = 0                  'Wenn keine Kühlung benötigt und der Lüfter aus gehen darf
            Temp_byte2 = 255 - F2_min
            Temp_single = Temp_byte2 / 100
            Temp_byte2 = Temp_single * Kuehlung2
            Temp_byte2 = Temp_byte2 + F2_min
            Fan2 = Temp_byte2


      End Select



  Else                                                                          'Wenn Manuelle Einstellung

      Select Case F2_status

         Case 0:                                                                'Lüfter ist aus

            Fan2 = 0                                                            'Lüfter aus
            If Manuell2 > 0 Then
               Incr F2_status                                                   'Lüfter soll anlaufen
               F2_anlauf_t = F2_anlauf                                          'Anlaufzeit zurücksetzen
            End If


         Case 1:                                                                'Lüfter läuft an

            Fan2 = 255                                                          'Vollgas

         Case 2:                                                                'Lüfter läuft Normal

            If Manuell2 = 0 Then
               F2_status = 0
            Else
               Fan2 = Manuell2
            End If



      End Select


  End If



'###############################################################################
'################################ Berechnung Fan3 ##############################
'###############################################################################


  If F3_auto = 1 Then                                                           'Wenn Lüfter automatisch geregelt werden soll

      Kuehlung3 = 0

      For Ctr = 1 To 24                                                         'berechne für jede Temperatur den Kühlungsbedarf

         If Temperaturen(ctr) >= Min3_t(ctr) And Temperaturen(ctr) <= Max3_t(ctr) Then       'Temperatur ist zwischen Min und Max

            Temp_byte = Max3_t(ctr) - Min3_t(ctr)
            Temp_byte = 100 / Temp_byte

            Temp_byte2 = Temperaturen(ctr) - Min3_t(ctr)
            Temp_byte2 = Temp_byte2 * Temp_byte

            If Temp_byte2 > Kuehlung3 Then Kuehlung3 = Temp_byte2               'Speichere den höchsten Kühlungsbedarf


         End If


         If Temperaturen(ctr) > Max3_t(ctr) Then                                'Temperatur ist Größer als Max

                  ' X X X F1 F2 F3 T D
                  Set Alarm_status.1

                  Kuehlung3 = 100


         End If

      Next




      Select Case F3_status

         Case 0:                                                                'Lüfter ist aus

            Fan3 = 0                                                            'Lüfter aus
            If F3_aus = 1 Then                                                  'Lüfter darf aus
               If Kuehlung3 > F3_anlauf_schwelle Then
                  Incr F3_status                                                'Lüfter soll anlaufen
                  F3_anlauf_t = F3_anlauf                                       'Anlaufzeit zurücksetzen
               End If
            Else                                                                'Lüfter sollte aber laufen
               Incr F3_status                                                   'Lüfter soll anlaufen
               F3_anlauf_t = F3_anlauf                                          'Anlaufzeit zurücksetzen
            End If


         Case 1:                                                                'Lüfter läuft an

            Fan3 = 255                                                          'Vollgas

         Case 2:                                                                'Lüfter läuft Normal

            If F3_aus = 1 And Kuehlung3 = 0 Then F3_status = 0                  'Wenn keine Kühlung benötigt und der Lüfter aus gehen darf
            Temp_byte2 = 255 - F3_min
            Temp_single = Temp_byte2 / 100
            Temp_byte2 = Temp_single * Kuehlung3
            Temp_byte2 = Temp_byte2 + F3_min
            Fan3 = Temp_byte2


      End Select



  Else                                                                          'Wenn Manuelle Einstellung

      Select Case F3_status

         Case 0:                                                                'Lüfter ist aus

            Fan3 = 0                                                            'Lüfter aus
            If Manuell3 > 0 Then
               Incr F3_status                                                   'Lüfter soll anlaufen
               F3_anlauf_t = F3_anlauf                                          'Anlaufzeit zurücksetzen
            End If


         Case 1:                                                                'Lüfter läuft an

            Fan3 = 255                                                          'Vollgas


         Case 2:                                                                'Lüfter läuft Normal

            If Manuell3 = 0 Then
               F3_status = 0
            Else
               Fan3 = Manuell3
            End If



      End Select


  End If






'###############################################################################
'############################### Befehle auswerten #############################
'###############################################################################

  Temp_4$ = Right(com_r , 2)                                                    'Schaut nach den letzten beiden Zeichen


  If Temp_4$ = Crlf Then                                                        'Wenn hinten Carrige Return und Line Feed steht, ist ein Befehl vollständig übertragen


      Temp_byte = Split(com_r , Datenwort(1) , "#")                             'Dann splitte den Empfangsstring in die jeweiligen Datenworte

      Com_r_ctr = 0                                                             'Setze den Zähler zurück, damit wieder empfangen werden kann
      Com_r = ""                                                                'und lösche erstmal den Empfangsstring, damit wir den nächsten Befehl empfangen können


      Decr Temp_byte                                                            'CRLF wird nicht mitgezählt

      If Temp_byte = Val(datenwort(2)) Then                                     'Wenn die angegebene Datenwortanzahl auch mit der empfangenen überein stimmt


         Select Case Datenwort(1)                                               'Führe den jeweiligen Befehl aus

            Case "EXIT":                                                        'Das GUI meldet sich ab (wird beendet)

               Gui_version = 0                                                  'Es gibt kein GUI mehr

               'Refresh wird abgeschaltet



            Case "INST":                                                        'Das GUI verlangt nach der Initialisierung

               Gui_version = Val(datenwort(3))                                  'Übernehme die GUI Version

               Print #1 , "INIT#279#" ; Pumpe_spannung ; "#" ; Dfm_imp ; "#" ;

               If Alarm_setup.0 = 1 Then                                        'Alarm für Durchfluss ist aktiviert
                  Print #1 , Durchfluss_alarm ; "#";
               Else                                                             'Alarm für Durchfluss ist nicht aktiviert
                  Print #1 , "0#";
               End If

               For Ctr = 1 To 24
                  Print #1 , Min1_t(ctr) ; "#";
               Next

               For Ctr = 1 To 24
                  Print #1 , Max1_t(ctr) ; "#";
               Next

               For Ctr = 1 To 24
                  Print #1 , Min2_t(ctr) ; "#";
               Next

               For Ctr = 1 To 24
                  Print #1 , Max2_t(ctr) ; "#";
               Next

               For Ctr = 1 To 24
                  Print #1 , Min3_t(ctr) ; "#";
               Next

               For Ctr = 1 To 24
                  Print #1 , Max3_t(ctr) ; "#";
               Next

               Print #1 , Manuell1 ; "#" ; F1_anlauf ; "#" ; F1_min ; "#" ; F1_auto ; "#" ; F1_aus ; "#";

               Print #1 , Manuell2 ; "#" ; F2_anlauf ; "#" ; F2_min ; "#" ; F2_auto ; "#" ; F2_aus ; "#";

               Print #1 , Manuell3 ; "#" ; F3_anlauf ; "#" ; F3_min ; "#" ; F3_auto ; "#" ; F3_aus ; "#";


               Temp_byte = Alarm_setup And &B00011100
               If Temp_byte = 0 Then                                            'Kein Lüfteralarm eingestellt
                  Print #1 , "0#";
               Else                                                             'ein Lüfteralarm wurde gefunden
                  Print #1 , "1#";
               End If

               For Ctr = 1 To 24                                                'Die 24 Temperaturnamen
                  Print #1 , Name_t(ctr) ; "#";
               Next

               Print #1 , Alarm_setup.1 ; "#";

               For Ctr = 1 To 8                                                 'Die 8 Ersatztemperaturen
                  Print #1 , Te(ctr) ; "#";
               Next

               For Ctr = 1 To 3                                                 'Welche Temperaturen angezeigt werden
                  Print #1 , Show(ctr) ; "#";
               Next

               Temp_byte = Led_back

               Print #1 , Temp_byte ; "#" ; Lcd_contrast ; "#" ; Show_dfm ; "#";

               Print #1 , Led_status ; "#";

               For Ctr = 1 To 3                                                 'Die 3 LED Werte
                  Print #1 , Led(ctr) ; "#";
               Next

               For Ctr = 1 To 63                                                'Die DS18S20 Adressen
                  Print #1 , Hex(adr_t(ctr)) ; "#";
               Next

               Print #1 , Hex(adr_t(64)) ; "#0#2#8#" ;                          'Firmware Version!

               Print #1 , F1_anlauf_schwelle ; "#" ; F2_anlauf_schwelle ; "#" ;
               Print #1 , F3_anlauf_schwelle ; "#" ; Pumpe_anlauf ; "#"


            Case "REST":                                                        'Reset

               If Datenwort(3) = "DOIT" Then
                  Cpu_ccp = &HD8
                  Rst_ctrl = 1
                  Wait 1
               End If


            Case "PCRE" :                                                       'Refresh vom PC

               Reset Gueltigkeit.4                                              'AIDA Werte empfangen

               For Ctr = 3 To 10
                   Temp_byte2 = Ctr + 14
                   Temperaturen(temp_byte2) = Val(datenwort(ctr))
               Next

               Cpu_takt_$ = Datenwort(14)
               Cpu_auslastung_$ = Datenwort(15)
               Gpu_auslastung_$ = Datenwort(16)
               Ram_auslastung_$ = Datenwort(17)




            Case "PUMP":                                                        'Pumpe

               Pumpe_spannung = Val(datenwort(3))

               Pumpe_anlauf = Val(datenwort(4))
               Temp_byte = Pumpe_anlauf_e
               If Pumpe_anlauf <> Temp_byte Then                                'Speichere falls geändert
                  Pumpe_anlauf_e = Pumpe_anlauf
               End If

               Temp_byte = Pumpe_spannung_e
               If Pumpe_spannung <> Temp_byte Then                              'Speichere falls geändert
                  Pumpe_spannung_e = Pumpe_spannung
               End If



            Case "DFM":                                                         'Durchflussmesser

               Dfm_imp = Val(datenwort(3))
               Temp_byte = Dfm_imp_e
               If Dfm_imp <> Temp_byte Then                                     'Speichere falls geändert
                  Dfm_imp_e = Dfm_imp
               End If

               Temp_word = Val(datenwort(4))
               If Temp_word = 0 Then
                  Reset Alarm_setup.0                                           'Deaktiviere Durchflussalarm
               Else
                  Set Alarm_setup.0                                             'Aktiviere Durchflussalarm
                  Durchfluss_alarm = Temp_word                                  'und weise ihm den Wert zu
                  Temp_word = Durchfluss_alarm_e
                  If Durchfluss_alarm <> Temp_word Then                         'Speichere falls geändert
                     Durchfluss_alarm_e = Durchfluss_alarm
                  End If
               End If

               If Datenwort(5) = "0" Then                                       'Temperatur Alarm
                  Reset Alarm_setup.1
               Else
                  Set Alarm_setup.1
               End If
               ' X X X F1 F2 F3 T D
               If Datenwort(6) = "0" Then                                       'Lüfter Alarm
                  Reset Alarm_setup.2
                  Reset Alarm_setup.3
                  Reset Alarm_setup.4
               Else
                  Set Alarm_setup.2
                  Set Alarm_setup.3
                  Set Alarm_setup.4
               End If

               Temp_byte = Alarm_setup_e
               If Alarm_setup <> Temp_byte Then                                 'Speichere falls geändert
                  Alarm_setup_e = Alarm_setup
               End If


            Case "MIA1":                                                        'Minmax Kanal 1

               For Ctr = 1 To 24
                  Temp_byte2 = Ctr + 2
                  Min1_t(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Min1_t_e(ctr)
                  If Min1_t(ctr) <> Temp_byte Then                              'Speichere falls geändert
                     Min1_t_e(ctr) = Min1_t(ctr)
                  End If
                  Temp_byte2 = Ctr + 26
                  Max1_t(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Max1_t_e(ctr)
                  If Max1_t(ctr) <> Temp_byte Then                              'Speichere falls geändert
                     Max1_t_e(ctr) = Max1_t(ctr)
                  End If
               Next


            Case "MIA2":                                                        'Minmax Kanal 2

               For Ctr = 1 To 24
                  Temp_byte2 = Ctr + 2
                  Min2_t(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Min2_t_e(ctr)
                  If Min2_t(ctr) <> Temp_byte Then                              'Speichere falls geändert
                     Min2_t_e(ctr) = Min2_t(ctr)
                  End If
                  Temp_byte2 = Ctr + 26
                  Max2_t(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Max2_t_e(ctr)
                  If Max2_t(ctr) <> Temp_byte Then                              'Speichere falls geändert
                     Max2_t_e(ctr) = Max2_t(ctr)
                  End If
               Next


            Case "MIA3":                                                        'Minmax Kanal 3

               For Ctr = 1 To 24
                  Temp_byte2 = Ctr + 2
                  Min3_t(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Min3_t_e(ctr)
                  If Min3_t(ctr) <> Temp_byte Then                              'Speichere falls geändert
                     Min3_t_e(ctr) = Min3_t(ctr)
                  End If
                  Temp_byte2 = Ctr + 26
                  Max3_t(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Max3_t_e(ctr)
                  If Max3_t(ctr) <> Temp_byte Then                              'Speichere falls geändert
                     Max3_t_e(ctr) = Max3_t(ctr)
                  End If
               Next


            Case "LUE1":                                                        'Lüfter Kanal 1

               Manuell1 = Val(datenwort(3))
               Temp_byte = Manuell1_e
               If Manuell1 <> Temp_byte Then                                    'Speichere falls geändert
                  Manuell1_e = Manuell1
               End If

               F1_anlauf = Val(datenwort(4))
               Temp_byte = F1_anlauf_e
               If F1_anlauf <> Temp_byte Then                                   'Speichere falls geändert
                  F1_anlauf_e = F1_anlauf
               End If

               F1_min = Val(datenwort(5))
               Temp_byte = F1_min_e
               If F1_min <> Temp_byte Then                                      'Speichere falls geändert
                  F1_min_e = F1_min
               End If

               If Datenwort(6) = "0" Then
                  Reset F1_auto
               Else
                  Set F1_auto
               End If
               If Datenwort(7) = "0" Then
                  Reset F1_aus
               Else
                  Set F1_aus
               End If
               Temp_byte = F_aus_auto_e
               If F_aus_auto <> Temp_byte Then                                  'Speichere falls geändert
                  F_aus_auto_e = F_aus_auto
               End If
               F1_anlauf_schwelle = Val(datenwort(8))
               Temp_byte = F1_anlauf_schwelle_e
               If F1_anlauf_schwelle <> Temp_byte Then                          'Speichere falls geändert
                  F1_anlauf_schwelle_e = F1_anlauf_schwelle
               End If



            Case "LUE2":                                                        'Lüfter Kanal 2

               Manuell2 = Val(datenwort(3))
               Temp_byte = Manuell2_e
               If Manuell2 <> Temp_byte Then                                    'Speichere falls geändert
                  Manuell2_e = Manuell2
               End If

               F2_anlauf = Val(datenwort(4))
               Temp_byte = F2_anlauf_e
               If F2_anlauf <> Temp_byte Then                                   'Speichere falls geändert
                  F2_anlauf_e = F2_anlauf
               End If

               F2_min = Val(datenwort(5))
               Temp_byte = F2_min_e
               If F2_min <> Temp_byte Then                                      'Speichere falls geändert
                  F2_min_e = F2_min
               End If

               If Datenwort(6) = "0" Then
                  Reset F2_auto
               Else
                  Set F2_auto
               End If
               If Datenwort(7) = "0" Then
                  Reset F2_aus
               Else
                  Set F2_aus
               End If
               Temp_byte = F_aus_auto_e
               If F_aus_auto <> Temp_byte Then                                  'Speichere falls geändert
                  F_aus_auto_e = F_aus_auto
               End If
               F2_anlauf_schwelle = Val(datenwort(8))
               Temp_byte = F2_anlauf_schwelle_e
               If F2_anlauf_schwelle <> Temp_byte Then                          'Speichere falls geändert
                  F2_anlauf_schwelle_e = F2_anlauf_schwelle
               End If


            Case "LUE3":                                                        'Lüfter Kanal 3

               Manuell3 = Val(datenwort(3))
               Temp_byte = Manuell3_e
               If Manuell3 <> Temp_byte Then                                    'Speichere falls geändert
                  Manuell3_e = Manuell3
               End If

               F3_anlauf = Val(datenwort(4))
               Temp_byte = F3_anlauf_e
               If F3_anlauf <> Temp_byte Then                                   'Speichere falls geändert
                  F3_anlauf_e = F3_anlauf
               End If

               F3_min = Val(datenwort(5))
               Temp_byte = F3_min_e
               If F3_min <> Temp_byte Then                                      'Speichere falls geändert
                  F3_min_e = F3_min
               End If

               If Datenwort(6) = "0" Then
                  Reset F3_auto
               Else
                  Set F3_auto
               End If
               If Datenwort(7) = "0" Then
                  Reset F3_aus
               Else
                  Set F3_aus
               End If
               Temp_byte = F_aus_auto_e
               If F_aus_auto <> Temp_byte Then                                  'Speichere falls geändert
                  F_aus_auto_e = F_aus_auto
               End If
               F3_anlauf_schwelle = Val(datenwort(8))
               Temp_byte = F3_anlauf_schwelle_e
               If F3_anlauf_schwelle <> Temp_byte Then                          'Speichere falls geändert
                  F3_anlauf_schwelle_e = F3_anlauf_schwelle
               End If


            Case "NAME":                                                        'Namen

               For Ctr = 1 To 24
                  Temp_byte2 = Ctr + 2
                  Name_t(ctr) = Datenwort(temp_byte2)
                  Temp_4$ = Name_t_e(ctr)
                  If Name_t(ctr) <> Temp_4$ Then                                'Speichere falls geändert
                     Name_t_e(ctr) = Name_t(ctr)
                  End If
               Next

               For Ctr = 1 To 3
                  Temp_byte2 = Ctr + 26
                  Show(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Show_e(ctr)
                  If Show(ctr) <> Temp_byte Then                                'Speichere falls geändert
                     Show_e(ctr) = Show(ctr)
                  End If
               Next


            Case "ERSA":                                                        'Ersatztemperaturen

               For Ctr = 1 To 8
                  Temp_byte2 = Ctr + 2
                  Te(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Te_e(ctr)
                  If Te(ctr) <> Temp_byte Then                                  'Speichere falls geändert
                     Te_e(ctr) = Te(ctr)
                  End If
               Next


            Case "ANZE":                                                        'Anzeige

               Temp_byte2 = Val(datenwort(3))
               Led_back = Temp_byte2
               Temp_byte = Led_back_e
               If Temp_byte2 <> Temp_byte Then                                  'Speichere falls geändert
                  Led_back_e = Temp_byte2
               End If

               Lcd_contrast = Val(datenwort(4))
               Temp_byte = Lcd_contrast_e
               If Lcd_contrast <> Temp_byte Then                                'Speichere falls geändert
                  Lcd_contrast_e = Lcd_contrast
               End If
               Gosub Lcd_kontrastset

               Show_dfm = Val(datenwort(5))
               Temp_byte = Show_dfm_e
               If Show_dfm <> Temp_byte Then                                    'Speichere falls geändert
                  Show_dfm_e = Show_dfm
               End If


            Case "LED":                                                         'Led

               Led_status = Val(datenwort(3))
               Temp_byte = Led_status_e
               If Led_status <> Temp_byte Then                                  'Speichere falls geändert
                  Led_status_e = Led_status
               End If

               For Ctr = 1 To 3
                  Temp_byte2 = Ctr + 3
                  Led(ctr) = Val(datenwort(temp_byte2))
                  Temp_byte = Led_e(ctr)
                  If Led(ctr) <> Temp_byte Then                                 'Speichere falls geändert
                     Led_e(ctr) = Led(ctr)
                  End If
               Next

               Gosub Led_init


         End Select


      End If


  End If




Loop





'###############################################################################
'################ Konvertiere Temp-Wert nach 4-Stelligen String ################
'###############################################################################

Function Convert_temp(byref Temperatur_wert As Byte) As String * 4

         Local X As Byte
         Local Temperatur_$ As String * 4

         X = Temperatur_wert
         Shift X , Right

         If X < 100 Then

             If X < 10 Then
                Temperatur_$ = " " + Str(x) + ","
             Else
                Temperatur_$ = Str(x) + ","
             End If

             If Temperatur_wert.0 = 1 Then
                Temperatur_$ = Temperatur_$ + "5"
             Else
                Temperatur_$ = Temperatur_$ + "0"
             End If

         Else

             Temperatur_$ = ">100"

         End If

         Convert_temp = Temperatur_$

End Function



Function Convert_4digit(byref Einword As Word) As String * 4

         Local Ergebnis_$ As String * 4

         Select Case Einword

            Case Is < 10 : Ergebnis_$ = "   " + Str(einword)

            Case 10 To 99 : Ergebnis_$ = "  " + Str(einword)

            Case 100 To 999 : Ergebnis_$ = " " + Str(einword)

            Case 1000 To 9999 : Ergebnis_$ = Str(einword)

            Case Else : Ergebnis_$ = "ERR."

         End Select

         Convert_4digit = Ergebnis_$

End Function



Function Convert_3digit(byref Einbyte As Byte) As String * 3

         Local Ergebnis_$ As String * 3

         Select Case Einbyte

            Case Is < 10 : Ergebnis_$ = "  " + Str(einbyte)

            Case 10 To 99 : Ergebnis_$ = " " + Str(einbyte)

            Case Else : Ergebnis_$ = Str(einbyte)

         End Select

         Convert_3digit = Ergebnis_$

End Function


'###############################################################################
'########################### Com Schnittstelle auslesen ########################
'###############################################################################

Recom:                                                                          'Wenn ein Byte per RS232 ankommt

   $asm

        CLI

        push r16                                                                'Alle verwendeten Register sichern
        Push r17
        push r20
        push r21
        PUSH r26                                                                'Das ist XL
        PUSH r27                                                                'Das ist XH
        in r20,SREG
        push r20                                                                'Das Statusregsiter hinterher


        IN r21,USARTC1_DATA                                                     'Als allererstes hohlen wir das empfangene Zeichen ab
        lds r16,{COM_r_ctr}                                                     'Lade den aktuellen Zählstand

        cpi r16, 253                                                            'Wenn Mein Zähler schon größer als 252 ist, dann empfange nichts mehr
        BRSH Skip_receiv

        Loadadr Com_r , X                                                       'Adresse vom Empfangsstring nach R26:r27

        ldi r17,0                                                               'R17 ist 0
        Add r26, r16                                                            'Addiere den aktuellen Zählerstand zu der Adresse
        adc r27,r17                                                             'Addiere zwar 0, aber auch das Carry Bit!

        'Jetzt steht der Zeiger an der richtigen Stelle

        st X+,R21                                                               'Speicher das empfangene Zeichen an der richigen Stelle
        st X,r17                                                                'und mache das Nächste Zeichen zum Stringende


        inc r16                                                                 'Jetzt den Zählstand um eins erhöhen
        STS {com_r_ctr},r16                                                     'und speichern

        LDS r20,{Gueltigkeit}                                                   'Gültigkeit
        CBR r20, 32                                                             'bestätigen
        STS {Gueltigkeit},r20                                                   'und speichern


Skip_receiv:

        POP r20
        Out Sreg , R20
        POP r27
        POP r26
        POP r21
        POP r20
        POP r17
        POP r16

        SEI

        $end Asm




Return



Get_digital_temperatures:


'############ 1-Wire auswerten (8 Sensoren) ############################

        Disable Interrupts                                                      'Die Interrupts müssen bei 1 Wire immer aus sein
        Temp_byte = 1wread(1)                                                   'Hier in die Klammern unbedingt die 1 rein!
        Enable Interrupts

        If Temp_byte = &HFF Then


           For Temp_byte = 1 To 1_wireanzahl

               Temp_word = Temp_byte
               Decr Temp_word
               Temp_word = Temp_word * 8

               Disable Interrupts
               1wreset
               Enable Interrupts

               Disable Interrupts
               1wwrite Match_rom
               Enable Interrupts

               For Ctr = 1 To 8
                   Temp_byte2 = Temp_word + Ctr

                   Disable Interrupts
                   1wwrite Adr_t(temp_byte2)
                   Enable Interrupts

               Next

               Disable Interrupts
               1wwrite Read_ram
               Enable Interrupts

               Disable Interrupts
               Temp_byte2 = 1wread(1)
               Enable Interrupts

               If Temp_byte2 = 255 Then

                  Temperaturen(temp_byte + 8) = 220                             'Mache das wenn kein Sensor angeschlossen ist

               Else

                  Temperaturen(temp_byte + 8) = Temp_byte2

               End If

           Next

           Disable Interrupts
           1wreset
           Enable Interrupts

           Disable Interrupts
           1wwrite Skip_rom
           Enable Interrupts

           Disable Interrupts
           1wwrite Convertt
           Enable Interrupts

        End If

Return




'###############################################################################
'####### Weist den LED Die Startwerte für die Automatischen Farbwechsel zu #####
'###############################################################################


Led_init:

   Select Case Led_status

       Case 0

            Led_r = Led(1)
            Led_g = Led(2)
            Led_b = Led(3)


       Case 1

            Led_r = 0
            Led_g = 0
            Led_b = 0


       Case 2 To 3

            Led_r = 0
            Led_g = 255
            Led_b = 0



   End Select

   Led_help = 0


Return







 '############## Displayroutine ######################################


 Display_akt:

     Select Case Screen

            Case 0:                                                             'Lüfter

                 Locate 1 , 1 : Lcd "1: " ; Convert_4digit(tacho1) ; "RPM  "
                 Temp_byte = Fan1
                 Temp_single = Temp_byte / 2.55
                 Temp_byte = Temp_single
                 Lcd Convert_3digit(temp_byte) ; "%"

                 Locate 2 , 1 : Lcd "2: " ; Convert_4digit(tacho2) ; "RPM  "
                 Temp_byte = Fan2
                 Temp_single = Temp_byte / 2.55
                 Temp_byte = Temp_single
                 Lcd Convert_3digit(temp_byte) ; "%"

                 Locate 3 , 1 : Lcd "3: " ; Convert_4digit(tacho3) ; "RPM  "
                 Temp_byte = Fan3
                 Temp_single = Temp_byte / 2.55
                 Temp_byte = Temp_single
                 Lcd Convert_3digit(temp_byte) ; "%"


            Case 1:

                 Locate 1 , 1 : Lcd " Pumpe:  "                                 'Pumpenspannung
                 If Pumpe_spannung < 100 Then Lcd " "
                 Temp_byte = Pumpe_spannung / 10
                 Lcd Temp_byte ; ","
                 Temp_byte = Pumpe_spannung Mod 10
                 Lcd Temp_byte ; " V "

                 Locate 2 , 1 : Lcd "  Durchfluss:   "                          'Durchfluss
                 Locate 3 , 1 : Lcd "    " ; Convert_4digit(durchfluss) ; " L/h    "


            Case 2:                                                             'PC Werte

                 'Dim Cpu_takt_$ As String * 4
'                 Dim Cpu_auslastung_$ As String * 3
'                 Dim Gpu_auslastung_$ As String * 3
'                 Dim Ram_auslastung_$ As String * 3

                 Locate 1 , 1 : Lcd " CPU:  " ; Cpu_takt_$ ; " MHz "
                 Locate 2 , 1 : Lcd "CPU:  GPU:  RAM:"
                 Locate 3 , 1 : Lcd Cpu_auslastung_$ ; "%  " ; Gpu_auslastung_$ ; "%  " ; Ram_auslastung_$ ; "%"


            Case 3:                                                             'Temperaturen

                 Locate 1 , 1
                 If Temp_ctr1 > 0 Then
                    Lcd " " ; Name_t(temp_ctr1) ; ":  "
                    Lcd Convert_temp(temperaturen(temp_ctr1)) ; " " ; Chr(&B11110010) ; "C "
                 Else
                    Lcd "                "
                 End If

                 Locate 2 , 1
                 If Temp_ctr2 > 0 Then
                    Lcd " " ; Name_t(temp_ctr2) ; ":  "
                    Lcd Convert_temp(temperaturen(temp_ctr2)) ; " " ; Chr(&B11110010) ; "C "
                 Else
                    Lcd "                "
                 End If

                 Locate 3 , 1
                 If Temp_ctr3 > 0 Then
                    Lcd " " ; Name_t(temp_ctr3) ; ":  "
                    Lcd Convert_temp(temperaturen(temp_ctr3)) ; " " ; Chr(&B11110010) ; "C "
                 Else
                    Lcd "                "
                 End If


     End Select


Return



'###############################################################################
'######################### Suche den nächsten Screen aus #######################
'###############################################################################



Next_screen:

       Timectr2 = 0

       If Screen = 2 Then Screen = 3

         If Screen = 1 Then

              If Show_aida > 0 Then
                 Screen = 2
              Else
                 Screen = 3
              End If

         End If


         If Screen = 0 Then

              If Show_dfm > 0 Then
                 Screen = 1
              Else
                If Show_aida > 0 Then
                   Screen = 2
                Else
                   Screen = 3
                End If
              End If

         End If


         If Temp_ctr = 25 Then                                                  'Ich habe alle abgearbeitet

                   Tempmask = 1                                                 'Setze die Maske zurück
                   Temp_ctr = 1                                                 'setze den Zähler zurück
                   Screen = 0                                                   'Gehe zum 1. Screen

         End If

         If Screen = 3 Then                                                     'wenn der Screen mit den Temperaturen dran ist

         'dann suche solange die nächsten 3 Temperaturen, bis es keine mehr gibt
         'und dann mache wieder den ersten screen!

              Temp_long = Show(3)                                               'Baue aus den 24 Bits ein Long
              Shift Temp_long , Left , 8
              Temp_long = Temp_long Or Show(2)
              Shift Temp_long , Left , 8
              Temp_long = Temp_long Or Show(1)                                  'Temp_long enthält jetzt die gewünschten temperaturen


              While Temp_ctr < 25

                Temp_long2 = Temp_long And Tempmask

                If Temp_long2 > 0 Then                                          'Ich habe eine Temperatur gefunden

                    Temp_ctr1 = Temp_ctr
                    Exit While

                End If

                Incr Temp_ctr
                Tempmask = Tempmask * 2

              Wend

              If Temp_ctr < 25 Then                                             'Er ist noch nicht am Ende
                 Incr Temp_ctr
                 Tempmask = Tempmask * 2
              Else                                                              'Er hat keinen mehr gefunden
                 Temp_ctr1 = 0
              End If

              While Temp_ctr < 25

                Temp_long2 = Temp_long And Tempmask

                If Temp_long2 > 0 Then                                          'Ich habe eine Temperatur gefunden

                    Temp_ctr2 = Temp_ctr
                    Exit While

                End If

                Incr Temp_ctr
                Tempmask = Tempmask * 2

              Wend

              If Temp_ctr < 25 Then                                             'Er ist noch nicht am Ende
                 Incr Temp_ctr
                 Tempmask = Tempmask * 2
              Else                                                              'Er hat keinen mehr gefunden
                 Temp_ctr2 = 0
              End If


              While Temp_ctr < 25

                Temp_long2 = Temp_long And Tempmask

                If Temp_long2 > 0 Then                                          'Ich habe eine Temperatur gefunden

                    Temp_ctr3 = Temp_ctr
                    Exit While

                End If

                Incr Temp_ctr
                Tempmask = Tempmask * 2

              Wend


              If Temp_ctr < 25 Then                                             'Er ist noch nicht am Ende
                 Incr Temp_ctr
                 Tempmask = Tempmask * 2

                 'Schaue nach ob noch eine kommt oder ob schon wieder zum 1. Screen gewechslet werden kann

                 Temp_byte = Temp_ctr
                 Temp_long3 = Tempmask

                 While Temp_byte < 25

                       Temp_long2 = Temp_long And Temp_long3

                       If Temp_long2 > 0 Then                                   'Ich habe eine Temperatur gefunden

                          Exit While                                            'Muss also noch weitergehen

                       End If

                       Incr Temp_byte
                       Temp_long3 = Temp_long3 * 2

                 Wend

                 If Temp_byte = 25 Then Temp_ctr = 25                           'Wenn die Whileschleife keine Temperatur mehr findet,
                 'dann wird der Zähler hochgesetzt


              Else                                                              'Er hat keinen mehr gefunden
                 Temp_ctr3 = 0
              End If



         End If

         Gosub Display_akt

Return



'################ ADC Auswerten #########################################

Get_analog_temperatures:

     For Ctr = 1 To 8


         Temp_byte = Ctr - 1

         Shift Temp_byte , Left , 3

         Adca_ch0_muxctrl = Temp_byte

         Temp_word = Getadc(adca , 0)

         If Temp_word = 4095 Then

            Temperaturen(ctr) = 220                                             'Mache das wenn kein Sensor angeschlossen ist

         Else

             Shift Temp_word , Right , 3

             Temperaturen(ctr) = Lookup(temp_word , Calculated_temp_values)

         End If

     Next

Return









'###############################################################################
'################### Runterzählen der Anlaufzeiten #############################
'###############################################################################


Count_startup_time:

     Select Case F1_anlauf_t

         Case Is > 1 :
            Decr F1_anlauf_t

         Case 1:
            Decr F1_anlauf_t
            Incr F1_status

     End Select

     Select Case F2_anlauf_t

         Case Is > 1 :
            Decr F2_anlauf_t

         Case 1:
            Decr F2_anlauf_t
            Incr F2_status

     End Select

     Select Case F3_anlauf_t

         Case Is > 1 :
            Decr F3_anlauf_t

         Case 1:
            Decr F3_anlauf_t
            Incr F3_status

     End Select

     Select Case Pumpe_anlauf_t

         Case Is > 1 :
            Decr Pumpe_anlauf_t

         Case 1:
            Decr Pumpe_anlauf_t
            Incr Pumpe_status

     End Select


Return





'###############################################################################
'################### Überprüfen ob Elemente noch aktuell sind ##################
'###############################################################################



Check_aida:


    If Gueltigkeit.4 = 1 Then                                                   'AIDA Temperaturen ungültig
       Show_aida = 0
       For Ctr = 1 To 8
         Temperaturen(ctr + 16) = Te(ctr)                                       'Dann verwende Ersatztemperaturen
       Next

    Else
       Show_aida = 1
    End If

    Set Gueltigkeit.4

Return



Check_gueltigkeit:


If Gueltigkeit.0 = 1 Then                                                       'DFM Wert ungültig
             Durchfluss = 0                                                     'kein Durchfluss
             'Alarm kein Durchfluss!
             ' X X X F1 F2 F3 T D
             If Pumpe_status = 2 Then                                           'Pumpe läuft normal
                  Set Alarm_status.0
             Else                                                               'Pumpe darf stehen
                  Reset Alarm_status.0
             End If

         Else                                                                   'DFM Wert gültig
             Incr Dfm_ctr                                                       'Berechne L/h
             Temp_long = Cv_dfm * Dfm_imp
             Temp_long = 225000000 / Temp_long                                  '2 Liter / Stunde
             Dfm_average(dfm_ctr) = Temp_long
             If Dfm_ctr = 10 Then Dfm_ctr = 0
             For Ctr = 1 To 10
                 Temp_long = Temp_long + Dfm_average(ctr)
             Next
             Durchfluss = Temp_long / 5

             If Durchfluss >= Durchfluss_alarm Then
                 Reset Alarm_status.0
             Else
                 If Pumpe_status = 2 Then Set Alarm_status.0
             End If

         End If


         If Gueltigkeit.1 = 1 Then                                              'cv_fan1 Ungültig
             Tacho1 = 0                                                         'Lüfter steht
             'Alarm Lüfter steht!
             ' X X X F1 F2 F3 T D
             If F1_status = 2 Then                                              'Lüfter soll aber normal laufen
                Set Alarm_status.4
             Else
                Reset Alarm_status.4
             End If

         Else                                                                   'Cv_fan1 gültig
                        'Berechne RPM
             Temp_long = Cv_fan1 * 2
             Temp_long = 7500000 / Temp_long
             Tacho1 = Temp_long
             Reset Alarm_status.4
         End If


         If Gueltigkeit.2 = 1 Then                                              'Cv_fan2 ungültig
             Tacho2 = 0                                                         'Lüfter steht
             'Alarm Lüfter steht!
             ' X X X F1 F2 F3 T D
            If F2_status = 2 Then                                               'Lüfter soll aber normal laufen
                Set Alarm_status.3
             Else
                Reset Alarm_status.3
             End If
         Else                                                                   'Cv_fan2 gültig
                        'Berechne RPM
             Temp_long = Cv_fan2 * 2
             Temp_long = 7500000 / Temp_long
             Tacho2 = Temp_long
             Reset Alarm_status.3
         End If


         If Gueltigkeit.3 = 1 Then                                              'Cv_fan3 ungültig
             Tacho3 = 0                                                         'Lüfter steht
             'Alarm Lüfter steht!
             ' X X X F1 F2 F3 T D
             If F3_status = 2 Then                                              'Lüfter soll aber normal laufen
                Set Alarm_status.2
             Else
                Reset Alarm_status.2
             End If
         Else                                                                   'Cv_fan3 gültig
                   'Berechne RPM
             Temp_long = Cv_fan3 * 2
             Temp_long = 7500000 / Temp_long
             Tacho3 = Temp_long
             Reset Alarm_status.2
         End If

         Disable Interrupts
         If Gueltigkeit.5 = 1 Then                                              'Wenn eine Sekunde lang keine neuen Daten eintreffen
             Com_r_ctr = 0
             Com_r = ""                                                         'Lösche Empfangsbuffer
         End If
         Enable Interrupts

         Gueltigkeit = Gueltigkeit Or &B11101111


Return





'################################ Alarm auswerten ##############################

Check_alarm:

         If Alarm_enable = 1 Then                                               'Wenn Alarm Global aktiviert ist

            Temp_byte = Alarm_status And Alarm_setup

            If Temp_byte > 0 Then
               Toggle Summer                                                    'Es gibt einen Alarm der aktiviert wurde!
            Else
                Reset Summer
            End If

         End If

Return

Alarm_toggle:

         If Alarm_enable = 1 Then                                               'Alarm aktiviert
             Alarm_enable = 0                                                   'Schalte ihn aus

             Set Summer
             Waitms 10
             Reset Summer
             Waitms 50
             Set Summer
             Waitms 10
             Reset Summer
             Waitms 50
             Set Summer
             Waitms 10
             Reset Summer


         Else

             Alarm_enable = 1

             Set Summer
             Waitms 100
             Reset Summer


         End If

Return


'###############################################################################
'############### Sende Refresh wenn Gui Initialisiert ist ######################
'###############################################################################

Send_refresh:

        If Gui_version > 0 Then                                                 'Init wurde durchgeführt

            Print #1 , "STRE#26#";

            For Ctr = 1 To 16
                Print #1 , Temperaturen(ctr) ; "#";
            Next

            Print #1 , Alarm_status.1 ; Alarm_status.2 ; Alarm_status.0 ; "#";
            Print #1 , Durchfluss ; "#" ; Tacho1 ; "#" ; Tacho2 ; "#" ; Tacho3 ; "#";
            Temp_byte = Fan1
            Print #1 , Temp_byte ; "#" ;
            Temp_byte = Fan2
            Print #1 , Temp_byte ; "#" ;
            Temp_byte = Fan3
            Print #1 , Temp_byte ; "#"

         End If

Return




'------------------------------------------------------------------------------
' LCD - Gosub-Routine: Lcd_contrastset
' Routine berechnen neue Kontrastwerte und steuert direkt den
' Kontroller des Display an.
'------------------------------------------------------------------------------
Lcd_kontrastset:                                                                ' Kontrasteinstellung Display


   ' Verarbeitung des Kontrastwertes für High-Byte und Low-Byte
   Tempvar_1 = Lcd_contrast And &B00001111
   Tempvar_1 = Tempvar_1 + &B01110000

   Tempvar_2 = Lcd_contrast
   Shift Tempvar_2 , Right , 4
   Tempvar_2 = Tempvar_2 And &B00000011
   Tempvar_2 = Tempvar_2 + &B01010100

   ' Instruction Table 1 einstellen [0,1]
   _temp1 = &B00101001
   !rCall _Lcd_control

   ' Tempvar_1 = &B0111xxxx für Kontrast Set Instruction Table 1 - Low Byte
   _temp1 = Tempvar_1
   !rCall _Lcd_control

   ' Temovar_2 = &B010101xx für Kontrast Set Instruction Table 1 - High Byte
   _temp1 = Tempvar_2
   !rCall _Lcd_control

   ' Zurückschalten auf Instruction Table 0 [0,0]
   _temp1 = &B00101000
   !rCall _Lcd_control
Return
'-- End Lcd_kontrastset -------------------------------------------------------



'###############################################################################
'###### ganz schnelle Assembler ISR's für die Periodendauermessung #############
'###############################################################################

C_dfm:

        'Tcc0_cca


        $asm

        CLI

        push r16                                                                'Alle verwendeten Register sichern
        Push r17
        push r20
        push r21
        in r20,SREG
        push r20                                                                'Das Statusregsiter hinterher

        in r16,{Tcc0_cca}                                                       '16 Bit Wert abholen
        in r17,{Tcc0_cca+1}
        lds r20,{Cv_dfm_alt_l}                                                  'Alten Wert abholen
        lds r21,{Cv_dfm_alt_h}
        sts {Cv_dfm_alt_l},r16                                                  'Alten Wert aktualisieren
        sts {Cv_dfm_alt_h},r17

        Sub R16 , R20                                                           'Differenz Bilden
        sbc r17,r21

        BRcs C_dfm_s                                                            'Wenn ergbenis der Differnz nicht negativ
        sts {Cv_dfm_l},r16                                                      'Wert zurückgeben
        STS {Cv_dfm_h},r17

        LDS r20,{Gueltigkeit}                                                   'Gültikkeit
        CBR r20, 1                                                              'bestätigen
        STS {Gueltigkeit},r20                                                   'und speichern

C_dfm_s:
        POP r20
        Out Sreg , R20
        POP r21
        POP r20
        POP r17
        POP r16

        SEI

        $end Asm


Return



C_fan1:

        'Tcc0_ccb

        $asm

        CLI

        push r16                                                                'Alle verwendeten Register sichern
        Push r17
        push r20
        push r21
        in r20,SREG
        push r20                                                                'Das Statusregsiter hinterher

        in r16,{Tcc0_ccB}                                                       '16 Bit Wert abholen
        in r17,{Tcc0_ccB+1}
        lds r20,{Cv_fan1_alt_l}                                                 'Alten Wert abholen
        lds r21,{Cv_fan1_alt_h}
        sts {Cv_fan1_alt_l},r16                                                 'Alten Wert aktualisieren
        sts {Cv_fan1_alt_h},r17

        Sub R16 , R20                                                           'Differenz Bilden
        sbc r17,r21

        BRCS C_fan1_s                                                           'Wenn ergbenis der Differnz nicht negativ
        sts {Cv_fan1_l},r16                                                     'Wert zurückgeben
        STS {Cv_fan1_h},r17

        LDS r20,{Gueltigkeit}                                                   'Gültikkeit
        CBR r20,2                                                               'bestätigen
        STS {Gueltigkeit},r20                                                   'und speichern

C_fan1_s:
        POP r20
        Out Sreg , R20
        POP r21
        POP r20
        POP r17
        POP r16

        SEI

        $end Asm

Return



C_fan2:

        'Tcc0_ccc

        $asm

        CLI

        push r16                                                                'Alle verwendeten Register sichern
        Push r17
        push r20
        push r21
        in r20,SREG
        push r20                                                                'Das Statusregsiter hinterher

        in r16,{Tcc0_ccc}                                                       '16 Bit Wert abholen
        in r17,{Tcc0_ccc+1}
        lds r20,{Cv_fan2_alt_l}                                                 'Alten Wert abholen
        lds r21,{Cv_fan2_alt_h}
        sts {Cv_fan2_alt_l},r16                                                 'Alten Wert aktualisieren
        sts {Cv_fan2_alt_h},r17

        Sub R16 , R20                                                           'Differenz Bilden
        sbc r17,r21

        BRCS C_fan2_s                                                           'Wenn ergbenis der Differnz nicht negativ
        sts {Cv_fan2_l},r16                                                     'Wert zurückgeben
        STS {Cv_fan2_h},r17

        LDS r20,{Gueltigkeit}                                                   'Gültikkeit
        CBR r20,4                                                               'bestätigen
        STS {Gueltigkeit},r20                                                   'und speichern

C_fan2_s:
        POP r20
        Out Sreg , R20
        POP r21
        POP r20
        POP r17
        POP r16

        SEI

        $end Asm

Return



C_fan3:

        'Tcc0_ccd

        $asm

        CLI

        push r16                                                                'Alle verwendeten Register sichern
        Push r17
        push r20
        push r21
        in r20,SREG
        push r20                                                                'Das Statusregsiter hinterher

        in r16,{Tcc0_ccd}                                                       '16 Bit Wert abholen
        in r17,{Tcc0_ccd+1}
        lds r20,{Cv_fan3_alt_l}                                                 'Alten Wert abholen
        lds r21,{Cv_fan3_alt_h}
        sts {Cv_fan3_alt_l},r16                                                 'Alten Wert aktualisieren
        sts {Cv_fan3_alt_h},r17

        Sub R16 , R20                                                           'Differenz Bilden
        sbc r17,r21

        BRCS C_fan3_s                                                           'Wenn ergbenis der Differnz nicht negativ
        sts {Cv_fan3_l},r16                                                     'Wert zurückgeben
        STS {Cv_fan3_h},r17

        LDS r20,{Gueltigkeit}                                                   'Gültikkeit
        CBR r20,8                                                               'bestätigen
        STS {Gueltigkeit},r20                                                   'und speichern

C_fan3_s:
        POP r20
        Out Sreg , R20
        POP r21
        POP r20
        POP r17
        POP r16

        SEI

        $end Asm

Return



Calculated_temp_values:                                                         '512 Temperaturwerte zum übersetzten des AD-Wertes/5



Data 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198
Data 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198
Data 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198
Data 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198 , 198
Data 198 , 197 , 195 , 193 , 191 , 189 , 187 , 185 , 183 , 182
Data 180 , 179 , 177 , 175 , 174 , 172 , 171 , 170 , 168 , 167
Data 166 , 164 , 163 , 162 , 161 , 159 , 158 , 157 , 156 , 155
Data 154 , 153 , 152 , 150 , 149 , 148 , 147 , 146 , 145 , 145
Data 144 , 143 , 142 , 141 , 140 , 139 , 138 , 137 , 137 , 136
Data 135 , 134 , 133 , 133 , 132 , 131 , 130 , 129 , 129 , 128
Data 127 , 127 , 126 , 125 , 124 , 124 , 123 , 122 , 122 , 121
Data 120 , 120 , 119 , 119 , 118 , 117 , 117 , 116 , 115 , 115
Data 114 , 114 , 113 , 113 , 112 , 111 , 111 , 110 , 110 , 109
Data 109 , 108 , 108 , 107 , 107 , 106 , 106 , 105 , 105 , 104
Data 104 , 103 , 103 , 102 , 102 , 101 , 101 , 100 , 100 , 99
Data 99 , 98 , 98 , 98 , 97 , 97 , 96 , 96 , 95 , 95
Data 94 , 94 , 94 , 93 , 93 , 92 , 92 , 92 , 91 , 91
Data 90 , 90 , 90 , 89 , 89 , 88 , 88 , 88 , 87 , 87
Data 87 , 86 , 86 , 85 , 85 , 85 , 84 , 84 , 84 , 83
Data 83 , 83 , 82 , 82 , 82 , 81 , 81 , 81 , 80 , 80
Data 80 , 79 , 79 , 79 , 78 , 78 , 78 , 77 , 77 , 77
Data 76 , 76 , 76 , 75 , 75 , 75 , 74 , 74 , 74 , 74
Data 73 , 73 , 73 , 72 , 72 , 72 , 71 , 71 , 71 , 71
Data 70 , 70 , 70 , 69 , 69 , 69 , 69 , 68 , 68 , 68
Data 68 , 67 , 67 , 67 , 66 , 66 , 66 , 66 , 65 , 65
Data 65 , 65 , 64 , 64 , 64 , 64 , 63 , 63 , 63 , 63
Data 62 , 62 , 62 , 62 , 61 , 61 , 61 , 61 , 60 , 60
Data 60 , 60 , 59 , 59 , 59 , 59 , 58 , 58 , 58 , 58
Data 57 , 57 , 57 , 57 , 57 , 56 , 56 , 56 , 56 , 55
Data 55 , 55 , 55 , 54 , 54 , 54 , 54 , 54 , 53 , 53
Data 53 , 53 , 53 , 52 , 52 , 52 , 52 , 51 , 51 , 51
Data 51 , 51 , 50 , 50 , 50 , 50 , 50 , 49 , 49 , 49
Data 49 , 49 , 48 , 48 , 48 , 48 , 48 , 47 , 47 , 47
Data 47 , 47 , 46 , 46 , 46 , 46 , 46 , 45 , 45 , 45
Data 45 , 45 , 44 , 44 , 44 , 44 , 44 , 43 , 43 , 43
Data 43 , 43 , 43 , 42 , 42 , 42 , 42 , 42 , 41 , 41
Data 41 , 41 , 41 , 41 , 40 , 40 , 40 , 40 , 40 , 39
Data 39 , 39 , 39 , 39 , 39 , 38 , 38 , 38 , 38 , 38
Data 38 , 37 , 37 , 37 , 37 , 37 , 36 , 36 , 36 , 36
Data 36 , 36 , 35 , 35 , 35 , 35 , 35 , 35 , 34 , 34
Data 34 , 34 , 34 , 34 , 33 , 33 , 33 , 33 , 33 , 33
Data 33 , 32 , 32 , 32 , 32 , 32 , 32 , 31 , 31 , 31
Data 31 , 31 , 31 , 30 , 30 , 30 , 30 , 30 , 30 , 30
Data 29 , 29 , 29 , 29 , 29 , 29 , 28 , 28 , 28 , 28
Data 28 , 28 , 28 , 27 , 27 , 27 , 27 , 27 , 27 , 26
Data 26 , 26 , 26 , 26 , 26 , 26 , 25 , 25 , 25 , 25
Data 25 , 25 , 25 , 24 , 24 , 24 , 24 , 24 , 24 , 24
Data 23 , 23 , 23 , 23 , 23 , 23 , 23 , 22 , 22 , 22
Data 22 , 22 , 22 , 22 , 21 , 21 , 21 , 21 , 21 , 21
Data 21 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 20 , 19
Data 19 , 19 , 19 , 19 , 19 , 19 , 18 , 18 , 18 , 18
Data 18 , 18