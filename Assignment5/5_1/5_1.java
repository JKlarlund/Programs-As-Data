import java.util.Arrays;

class Main{
  static int[] merge(int[] xs, int[] ys){

    int[] zs = new int[xs.length+ys.length];
    
    for (int i = 0, j=0, k=0; k<zs.length; k++){
      if (i>=xs.length){
        for(int x = k, y = j; x<zs.length; x++, y++){
          zs[x] = ys[y];
        }
        break;
      }
      else if(j>=ys.length){
        for(int x = k, y=i; x<zs.length; x++, y++){
          zs[x] = xs[y];    
        }
        break;
      }
      else{
        if (xs[i] < ys[j]){
          zs[k]=xs[i];
          i++;
        }
        else{
          zs[k] = ys[j];
          j++;
        }
      }
    }
    return zs;
  }

  public static void main(String[] args){
    int[] xs = { 3, 5, 12 };
    int[] ys = { 2, 3, 4, 7 };
    
    System.out.println(Arrays.toString(merge(xs, ys)));

  }
}
