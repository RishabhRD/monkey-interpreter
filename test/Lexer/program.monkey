let add = fn(a, b){
  return a + b;
}

let main = fn(){
  let welcomeString = "Hello, \"Rishabh<rishabhdwivedi17@gmail.com>\"!";
  let regardsString = "Thanks,

  Regards, Monkey";
  let x = 1 + 2 - 3 * 4 / 5 % 6 ^ 71 | 8 & 9;
  let y = ~10;
  let sum = add(x, y);
  let condition = x + y >= sum;
  if(condition){
    print("The condition was not " + false);
  }else{
    print("The condition was not " + true);
  }

  if(21 > 22){
    print("No way");
  }else{
    print("Good");
  }
  if(21 < 22){
    print("Good");
  }else{
    print("No Way");
  }
  if(21 <= 22){
    print("Good");
  }else{
    print("No Way");
  }
  if(21 >= 22){
    print("No way");
  }else{
    print("Good");
  }
  if(21 == 22){
    print("No way");
  }else{
    print("Good");
  }
  if(!(21 == 22 && 22 == 23 || 24 == 24)){
    print("Don't know what is it");
  }else{
    print("Don't know");
  }
}
