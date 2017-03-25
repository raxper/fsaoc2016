class MD5Debug
{
 
  private static int INIT_A = 0x67452301;
  private static int INIT_B = (int)0xEFCDAB89L;
  private static int INIT_C = (int)0x98BADCFEL;
  private static int INIT_D = 0x10325476;
 
  private static int[] SHIFT_AMTS = new int[] {
    7, 12, 17, 22,
    5,  9, 14, 20,
    4, 11, 16, 23,
    6, 10, 15, 21
  };
 
  private static int[] TABLE_T = new int[64];
  static
  {
    for (int i = 0; i < 64; i++)
      TABLE_T[i] = (int)(long)((1L << 32) * Math.abs(Math.sin(i + 1)));
  }
 
  public static byte[] computeMD5(byte[] message)
  {
    int messageLenBytes = message.length;
    int numBlocks = ((messageLenBytes + 8) >>> 6) + 1;
    int totalLen = numBlocks << 6;
    byte[] paddingBytes = new byte[totalLen - messageLenBytes];
    paddingBytes[0] = (byte)0x80;
 
    long messageLenBits = (long)messageLenBytes << 3;
    for (int i = 0; i < 8; i++)
    {
      paddingBytes[paddingBytes.length - 8 + i] = (byte)messageLenBits;
      messageLenBits >>>= 8;
    }
    System.out.println("Input message is " + messageLenBytes + " byte(s): " + toHexArray(message));
    System.out.println("Padding length is " + paddingBytes.length + " bytes: " + toHexArray(paddingBytes));
    System.out.println("Total number of 512-bit (16 word) blocks: " + numBlocks);
 
    int a = INIT_A;
    int b = INIT_B;
    int c = INIT_C;
    int d = INIT_D;
    int[] buffer = new int[16];
    System.out.println("Initializing A=" + toHexString(a) + " B=" + toHexString(b) + " C=" + toHexString(c) + " D=" + toHexString(d));
    for (int i = 0; i < numBlocks; i ++)
    {
      int index = i << 6;
      for (int j = 0; j < 64; j++, index++)
        buffer[j >>> 2] = ((int)((index < messageLenBytes) ? message[index] : paddingBytes[index - messageLenBytes]) << 24) | (buffer[j >>> 2] >>> 8);
      System.out.println("Starting processing on block #" + i + ": " + toHexArray(buffer));
      int originalA = a;
      int originalB = b;
      int originalC = c;
      int originalD = d;
      System.out.println("  Before all rounds: A=" + toHexString(a) + " B=" + toHexString(b) + " C=" + toHexString(c) + " D=" + toHexString(d) + "; saving as AA/BB/CC/DD");
      for (int j = 0; j < 64; j++)
      {
        int mod16 = j & 0x0F;
        int div16 = j >>> 4;
        int f = 0;
        int bufferIndex = j;
        if (mod16 == 0)
          System.out.println("  Round " + (div16 + 1) + ", using " + "FGHI".charAt(div16) + "()");
        switch (div16)
        {
          case 0:
            f = (b & c) | (~b & d);
            System.out.print("f:" + padTwo(f) + ",buffer:" + padTwo(buffer[bufferIndex]) + ",lshift:" + padTwo(SHIFT_AMTS[(div16 << 2) | (j & 3)]));
            break;
 
          case 1:
            f = (b & d) | (c & ~d);
            bufferIndex = (bufferIndex * 5 + 1) & 0x0F;
            break;
 
          case 2:
            f = b ^ c ^ d;
            bufferIndex = (bufferIndex * 3 + 5) & 0x0F;
            break;
 
          case 3:
            f = c ^ (b | ~d);
            bufferIndex = (bufferIndex * 7) & 0x0F;
            break;
        }
        int temp = b + Integer.rotateLeft(a + f + buffer[bufferIndex] + TABLE_T[j], SHIFT_AMTS[(div16 << 2) | (j & 3)]);
        System.out.print(" pre-rotate:" + padTwo(a + f + buffer[bufferIndex] + TABLE_T[j])+",post-rotate:"+Integer.rotateLeft(a + f + buffer[bufferIndex] + TABLE_T[j], SHIFT_AMTS[(div16 << 2) | (j & 3)]));
        a = d;
        d = c;
        c = b;
        b = temp;
 

        int[] debugVars = { a, b, c, d };
        System.out.println("    Applying [" + "BCDABCD".substring(3 - (j & 3), 7 - (j & 3)) + " " + padTwo(bufferIndex) + " " + padTwo(SHIFT_AMTS[(div16 << 2) | (j & 3)]) + " " + padTwo(j + 1) + "]: A=" + toHexString(debugVars[(j + 1) & 3]) + " B=" + toHexString(debugVars[(j + 2) & 3]) + " C=" + toHexString(debugVars[(j + 3) & 3]) + " D=" + toHexString(debugVars[j & 3]) + " T[" + j + "]=" + toHexString(TABLE_T[j]));
       if (((j + 1) & 3) == 0)
         System.out.println();
      }
 
      a += originalA;
      b += originalB;
      c += originalC;
      d += originalD;
      System.out.println("  After adding original values (AA/BB/CC/DD): A=" + toHexString(a) + " B=" + toHexString(b) + " C=" + toHexString(c) + " D=" + toHexString(d));
    }
 
    byte[] md5 = new byte[16];
    int count = 0;
    for (int i = 0; i < 4; i++)
    {
      int n = (i == 0) ? a : ((i == 1) ? b : ((i == 2) ? c : d));
      for (int j = 0; j < 4; j++)
      {
        md5[count++] = (byte)n;
        n >>>= 8;
      }
    }
    System.out.println("[A B C D]: " + toHexArray(md5));
    return md5;
  }
 
  public static String padTwo(int n)
  {
    String s = String.valueOf(n);
    if (s.length() == 1)
      s = " " +s;
    return s;
  }
 
  public static String toHexString(int n)
  {
    char[] outputChars = new char[8];
    String HEX_CHARS = "0123456789ABCDEF";
    for (int i = 7; i >= 0; i--)
    {
      outputChars[i] = HEX_CHARS.charAt(n & 0x0F);
      n >>>= 4;
    }
    return new String(outputChars);
  }
 
  public static String toHexString(byte b)
  {
    String HEX_CHARS = "0123456789ABCDEF";
    int n = (int)b & 0xFF;
    return new String(new char[] { HEX_CHARS.charAt(n >>> 4), HEX_CHARS.charAt(n & 0x0F) } );
  }
 
  public static String toHexString(byte[] b)
  {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < b.length; i++)
      sb.append(toHexString(b[i]));
    return sb.toString();
  }
 
  public static String toHexArray(byte[] b)
  {
    StringBuilder sb = new StringBuilder("[ ");
    for (int i = 0; i < b.length; i++)
    {
      if (i > 0)
        sb.append(", ");
      sb.append(toHexString(b[i]));
    }
    sb.append(" ]");
    return sb.toString();
  }
 
  public static String toHexArray(int[] b)
  {
    StringBuilder sb = new StringBuilder("[ ");
    for (int i = 0; i < b.length; i++)
    {
      if (i > 0)
        sb.append(", ");
      sb.append(toHexString(b[i]));
    }
    sb.append(" ]");
    return sb.toString();
  }
 
  public static void main(String[] args)
  {
    String[] testStrings = { "", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "12345678901234567890123456789012345678901234567890123456789012345678901234567890" };
    if (args.length > 0)
      testStrings = args;
    for (String s : testStrings)
    {
      System.out.println("Calculating MD5 on \"" + s + "\"");
      byte[] md5 = computeMD5(s.getBytes());
      System.out.println("MD5 Hash: " + toHexString(md5));
      System.out.println();
    }
    return;
  }
 
}