#define DIGIT_0 0x3f
#define DIGIT_1 0x06
#define DIGIT_2 0x5b
#define DIGIT_3 0x4f
#define DIGIT_4 0x66
#define DIGIT_5 0x6d
#define DIGIT_6 0x7d
#define DIGIT_7 0x07
#define DIGIT_8 0x7f
#define DIGIT_9 0x6f
#define DIGIT_A 0x77
#define DIGIT_b 0x7c
#define DIGIT_C 0x39
#define DIGIT_d 0x5E
#define DIGIT_E 0x79
#define DIGIT_F 0x71
#define DIGIT__ 0x40
#define DIGIT_r 0x50
#define DIGIT_o 0x5c
#define DIGIT_P 0x73
#define DIGIT_S 0x6d
#define DIGIT_I 0x30
#define DIGIT_u 0x1c
#define DIGIT_t 0x70
#define DIGIT_n 0x54
#define DIGIT_dot 0x80
#define DIGIT_G 0x3E
#define DIGIT_L 0x38
#define DIGIT_J 0xF
const byte button_one[] = {DIGIT_b, DIGIT_u, DIGIT_t, DIGIT_t, DIGIT_o, DIGIT_n, DIGIT__, DIGIT_1};
const byte button_two[] = {DIGIT_b, DIGIT_u, DIGIT_t, DIGIT_t, DIGIT_o, DIGIT_n, DIGIT__, DIGIT_2};
const byte info_continue[] = {DIGIT_C, DIGIT_o, DIGIT_n, DIGIT_t, DIGIT_I, DIGIT_n, DIGIT_u, DIGIT_E};
const byte pause[] = {DIGIT_P, DIGIT_A, DIGIT_u, DIGIT_S, DIGIT_E, DIGIT_dot, DIGIT_dot, DIGIT_dot};
const byte starting[] = {DIGIT_I, DIGIT_n, DIGIT_I, DIGIT_t, DIGIT_I, DIGIT_A, DIGIT_L, DIGIT_dot};
const byte error_code[] = {DIGIT_E, DIGIT_r, DIGIT_r, DIGIT__, DIGIT_C, DIGIT_o, DIGIT_d, DIGIT_E};
const byte error_times[] = {DIGIT_E, DIGIT_r, DIGIT_r, DIGIT__, DIGIT_r, DIGIT_E, DIGIT_P, DIGIT_S};
const byte error_disp[] = {DIGIT_E, DIGIT_r, DIGIT_r, DIGIT__, DIGIT_d, DIGIT_I, DIGIT_S, DIGIT_P}; 
const byte load_code[] = {DIGIT_L, DIGIT_o, DIGIT_A, DIGIT_d, DIGIT_C, DIGIT_o, DIGIT_d, DIGIT_E};
const byte load_times[] = {DIGIT_L, DIGIT_o, DIGIT_A, DIGIT_d, DIGIT_r, DIGIT_E, DIGIT_P, DIGIT_S};
const byte load_disp[] = {DIGIT_L, DIGIT_o, DIGIT_A, DIGIT_d, DIGIT_d, DIGIT_I, DIGIT_S, DIGIT_P}; 
byte load_times_val[] = {DIGIT_r, DIGIT_E, DIGIT_P, DIGIT_S, DIGIT_dot , DIGIT_dot, DIGIT_dot,DIGIT_dot};
byte load_disp_val[] = {DIGIT_d, DIGIT_I, DIGIT_S, DIGIT_P, DIGIT_dot, DIGIT_dot, DIGIT_dot, DIGIT_dot}; 
const byte serial[] = {DIGIT_S, DIGIT_E, DIGIT_r, DIGIT_I, DIGIT_A, DIGIT_L, DIGIT_dot, DIGIT_dot}; 
const byte accepted[] = {DIGIT_A, DIGIT_C, DIGIT_C, DIGIT_E, DIGIT_P, DIGIT_t, DIGIT_E, DIGIT_d}; 
const byte heheszki[] = {DIGIT_A, DIGIT_L, DIGIT_E, DIGIT__, DIGIT_J, DIGIT_A, DIGIT_J, DIGIT_A}; /*do usunięcia*/
byte board[] = {DIGIT_0, DIGIT_1, DIGIT_2, DIGIT_3, DIGIT_4, DIGIT_5, DIGIT_6, DIGIT_7, DIGIT_8, DIGIT_9, DIGIT_A, DIGIT_b, DIGIT_C, DIGIT_d, DIGIT_E, DIGIT_F, };                 
const int stb = 10;
const int clk = 9;
const int dio = 8;                 
byte digits[115];
byte text[8];   /*docelowy text wypisywany w funkcji printText*/
char CODE[]="ALE-JAJA"; /*Domyślny Kod Hex*/
int TIMES = 1;    /**/
int DISP = 250;   /**/
int i = 0;
int n = 0;
int code = 1;
int range;  /*Zakres obrotów petli*/
bool completed = false;

void sendCommand(byte value){
  digitalWrite(stb,LOW);
  shiftOut(dio,clk,LSBFIRST,value);
  digitalWrite(stb,HIGH);
}
 
void reset(){
  sendCommand(0x40);
  digitalWrite(stb,LOW);
  shiftOut(dio,clk,LSBFIRST,0xc0);
  for(byte i=0; i<16; i++){
    shiftOut(dio,clk,LSBFIRST,0x00);
  }
  digitalWrite(stb,HIGH);
}
 
void setup() {
  pinMode(stb, OUTPUT);
  pinMode(clk,OUTPUT);
  pinMode(dio, OUTPUT);
  initialize();
  sendCommand(0x8F);
  reset();
  Serial.begin(9600); /*Ustawienie prędkości transmisji*/
  Serial.println("Team - Piotr Margański, Mateusz Wiśniewski, Aleksy Wołowiec - buuuja Kurwa!!!"); //Jednorazowe wysłanie tekstu  /*Wywalić BUUUJA KURWA!!!*/
  printError(starting);
  read_code();
  delay(3000);
}
 
void loop() {    
  proceed();
  read_serial();
}

void proceed(){
  code *= action_listener(pressedButton());
  range = 16*TIMES;
  
  if(i == 8){
      i = 0;
      completed = true;
  }
  if((code > 0)&&(!completed)){ 
    
    if((n<range)){
      changing(i);
      n++;
    }
    
    if((i<8)&&(n == range)){
      printText(i);
      lightsOn(i);
      i++;
      n=0;
    }
  } 
}
 
bool printText(byte i){
  for(byte position = 0; position < 8; position++)
  {
    sendCommand(0x44);
    digitalWrite(stb, LOW);
    shiftOut(dio, clk, LSBFIRST, 0xC0 + (position << 1));
    shiftOut(dio, clk, LSBFIRST, text[position]); 
    digitalWrite(stb, HIGH);
  }
}
 
bool changing(byte i){
  byte position = i;
  static byte index = 0;
  sendCommand(0x40);
  digitalWrite(stb,LOW);
  shiftOut(dio, clk, LSBFIRST, 0xc0 + (position << 1));
  for (position; position < 8; position++){
    shiftOut(dio, clk, LSBFIRST, board[index]);
    shiftOut(dio, clk, LSBFIRST, 0x00);
  }
  digitalWrite(stb,HIGH);
  delay(DISP);
  index = ++index % 16;
  return index == 0;
}
 
void lightsOn(byte i){
  digitalWrite(stb, LOW);
  /*Serial.print("Led ");
  Serial.print(i);
  Serial.print(" on!  ");*/
  shiftOut(dio, clk, LSBFIRST, 0xc1+(i*2)); // led on
  shiftOut(dio, clk, LSBFIRST, 1);
  digitalWrite(stb, HIGH);
}
void lightsOff(byte i){
  digitalWrite(stb, LOW);
  /*Serial.print("Led ");
  Serial.print(i);
  Serial.print(" off!  ");*/
  shiftOut(dio, clk, LSBFIRST, 0xc1+(i*2)); // led off
  shiftOut(dio, clk, LSBFIRST, 0);
  digitalWrite(stb, HIGH);
}

uint8_t readButtons(void){
  
  uint8_t buttons = 0;
  digitalWrite(stb, LOW);
  shiftOut(dio, clk, LSBFIRST, 0x42);

  pinMode(dio, INPUT);
 
  for (uint8_t i = 0; i < 4; i++)
  {
    uint8_t v = shiftIn(dio, clk, LSBFIRST) << i;
    buttons |= v;
  }
 
  pinMode(dio, OUTPUT);
  digitalWrite(stb, HIGH);
  return buttons;
}

int pressedButton(){
  
  uint8_t buttons = readButtons();
  for(uint8_t position = 0; position < 8; position++)
  {
    uint8_t mask = 0x1 << position;    
    if (buttons & mask) {     
      return position;   
    }
  }
  return -1;
}

int action_listener( int button_number){  
  if(button_number == 0){
    
    Serial.println("Button1 - Action");
    if(!completed){
      printError(button_one);
      delay(1000);
      if(code < 0){
        printError(info_continue);
        // reset();
        for(int j = 0; j < i; j++){
          lightsOn(j);
        }
        n = 16*TIMES;
        i-=1;
      }else{
        printError(pause);
        wavingleds();
      }
        delay(800);
        return -1; 
    }
    return 1;                           /* Naciśnięcie w trakcie procesu łamania kodu przycisku S1 powoduje wstrzymanie łamania, a kolejne  naciśnięcie wznawia łamanie*/
  }
  if(button_number ==1){ 
     
    Serial.println("Button2 - Action");  /*naciśnięcie przycisku S2 – wtedy ponownie łamany jest dotychczasowy kod  naciskanie przycisku S2 w trakcie łamania nie powinna powodować żadnej akcji. */
    if(completed){
      printError(button_two);
      completed = false;    
      delay(1000);
      printError(info_continue);
      //reset();
      delay(1000);
      printError(starting);
      delay(1000);
      for(int j = 0; j < i; j++){
        lightsOn(j);
      }
      n = 16*TIMES;
      i-=1;
    }
  }
  return 1;
}

void read_code(){   
   for(int i = 0; i < 8; i++){
     text[i] = digits[(int)(CODE[i])];
     Serial.print(CODE[i]);
   }    
}

void initialize(){
  
  digits[48] = DIGIT_0;
  digits[49] = DIGIT_1;
  digits[50] = DIGIT_2;
  digits[51] = DIGIT_3;
  digits[52] = DIGIT_4;
  digits[53] = DIGIT_5;
  digits[54] = DIGIT_6;
  digits[55] = DIGIT_7;
  digits[56] = DIGIT_8;
  digits[57] = DIGIT_9;
  digits[65] = DIGIT_A;
  digits[98] = DIGIT_b;
  digits[67] = DIGIT_C;
  digits[100] = DIGIT_d;
  digits[69] = DIGIT_E;
  digits[70] = DIGIT_F;
  digits[76] = DIGIT_L;
  digits[45] = DIGIT__;
  digits[74] = DIGIT_J;
}

void read_serial(){
  
  bool error= false;
  String Data = "";
  if(Serial.available() > 0) { //Czy Arduino odebrało dane
    printError(serial);
    delay(1000);
    Data = Serial.readStringUntil('\n'); //Jeśli tak, to odczytaj je do znaku końca linii i zapisz w zmiennej Data
    Serial.println("Odebrano  " + Data + " !"); //Wyświetl komunikat

    if((Data.length() == 2)||(Data.length() == 1)){  //Prawdopodobnie timesy - repsy
      for(int j = 0; j< Data.length(); j++){
        if((((int)(Data[j])) < 48)||(((int)(Data[j])) > 57)){
          error = true;
          Serial.println("error");
          completed = true;
          printError(error_times);
          wavingleds();
          printError(error_code);
          wavingleds();
          break;
        } 
      }
      if(!error){
        TIMES = Data.toInt();
        printError(accepted);
        delay(500);
        printError(load_times);
        shakingleds();
        compute_vals(load_times_val,Data);
        reset();
        for(int j = 0; j < i; j++){
          lightsOn(j);
        }
        n = 16*TIMES;
        i-=1;
      }
    }else if((Data.length() <= 4)&&(Data.length() >= 3)){  //Prawdopodobnie displajsy
      for(int j = 0; j< Data.length(); j++){
        if((((int)(Data[j])) < 48)||(((int)(Data[j])) > 57)){
          error = true;
          Serial.println("error");
          completed = true;
          printError(error_disp);
          wavingleds();
          printError(error_code);
          wavingleds();
          break;
        } 
      }
      if(!error){
        DISP = Data.toInt();
        printError(accepted);
        delay(500);
        printError(load_disp);
        shakingleds();
        compute_vals(load_disp_val,Data);
        reset();
        for(int j = 0; j < i; j++){
          lightsOn(j);
        }
        n = 16*TIMES;
        i-=1;
      }
    }else if(Data.length() == 8){  //Prawdopodobnie codesy
      for(int j = 0; j< Data.length(); j++){
        if(((((int)(Data[j])) < 48)||(((int)(Data[j])) > 57)&&((((int)(Data[j])) < 65)||(((int)(Data[j])) > 70)))&&((int)(Data[j])!=98)&&((int)(Data[j])!=100)){
          error = true;
          completed = true;
          Serial.println((int)(Data[j]));
          printError(error_code);
          wavingleds();
          break;
        } 
      }
      if(!error){
        Data.toCharArray(CODE, 9) ;
        read_code();
        printError(accepted);
        delay(1200);
        printError(load_code);
        shakingleds();
        printError(starting);
        delay(1000);
        completed = false;
        reset();
        /*for(int j = 0; j < i; j++){
          lightsOn(j);
        }*/
        // n = 16*TIMES;
        i= 0;
      }
    }else{
       printError(error_code);
       wavingleds();
       completed = true;
        //reset();
        for(int j = 0; j < i; j++){
          lightsOn(j);
        }
        i= 0;
     }
  } 
}

void wavingleds(){
  
   int side = 1;
   for(int leds =0; leds < 4; leds++){
     if((side % 2) == 1){
      for(int j = 0; j < 8; j++){
        lightsOn(j);
        delay(80); 
      }
     }else{
       for(int j = 7; j >= 0; j--){
        lightsOn(j);
        delay(80); 
      }
     }
     delay(200);
     side++;
     for(int j = 0; j < 8; j++){
       lightsOff(j);
     }
   }
}
void shakingleds(){
  for(int leds =0; leds < 40; leds++){
    
      for(int j = 0; j < 8; j++){
        lightsOn(j);
        delay(5);
      }
     
      for(int j = 0; j < 8; j++){
        lightsOff(j);
        delay(5);
      }
  }
}
void printError(const byte *board){
  
  //reset();
  int position;
  sendCommand(0x40);
  digitalWrite(stb,LOW); 
  for (position = 0; position < 8; position++){
    shiftOut(dio, clk, LSBFIRST, 0xc0 + (position << 1));
    shiftOut(dio, clk, LSBFIRST, board[position]);
    //shiftOut(dio, clk, LSBFIRST, 0x00);
  }
  digitalWrite(stb,HIGH);
}
void compute_vals(byte *board, String val){
   for(int i = 0; i < val.length(); i++){
     board[i+ 4] = digits[(int)(val[i])];
     Serial.print(val[i]);
   }  
   printError(board);
   shakingleds(); 
   for(int i = 4; i < 8; i++){
     board[i] = DIGIT_dot;
   }  
}

