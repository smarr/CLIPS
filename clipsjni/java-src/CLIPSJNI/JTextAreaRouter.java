package CLIPSJNI;

import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
import java.io.Reader;
import java.io.InputStreamReader;

public class JTextAreaRouter implements Router, KeyListener
  {  
   protected Environment clips;
   protected JTextArea jta;
   protected StringBuffer buffer; 
   private boolean expectingInput;
   private int charInput;
   private long bufferInputCount;
   
   private Reader theReader;
   private boolean ungotten;
   private int lastChar;
   private int numChar;
   private int curChar;
   private int char2;
   private int char3;
   
   private int maxLines;
   
   String readInputBuffer;
   int readInputLength;
   int readInputPosition;
      
   /*******************/
   /* JTextAreaRouter */
   /*******************/
   public JTextAreaRouter(
     Environment theEnv) throws Exception
     {  
      clips = theEnv;
      expectingInput = false;
      
      jta = new JTextArea();
      jta.setEditable(false);
      jta.addKeyListener(this);
      
      buffer = new StringBuffer(); 
      
      ungotten = false;
      numChar = 0;
      curChar = 0;
      theReader = new InputStreamReader(System.in,"UTF-8");
      
      maxLines = 1000;
      
      readInputBuffer = null;
      readInputLength = 0;
      readInputPosition = 0;
     }  
  
  /* Should following methods be public? */
     
   public JTextArea getJTextArea() 
     {
      return jta;
     }
     
   public void keyPressed(KeyEvent e) 
     {
      /* if (! expectingInput) return; */
      
      if (e.getID() != KeyEvent.KEY_PRESSED) return;
      
      if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return;

      e.consume();
     }

   public void keyReleased(KeyEvent e) 
     { 
      /* if (! expectingInput) return; */

      if (e.getID() != KeyEvent.KEY_RELEASED) return;
      
      if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return;

      e.consume();
     }

   public void keyTyped(KeyEvent e) 
     {
      synchronized (this)
        {
         if (! expectingInput) return;
      
         int id = e.getID();
      
         if (id != KeyEvent.KEY_TYPED) return;

         if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return;
                  
         char c = e.getKeyChar();

         charInput = c;
         
         if (charInput == KeyEvent.VK_BACK_SPACE)
           {
            if (bufferInputCount <= 0) return;
            jta.append(buffer.toString());
            buffer = new StringBuffer();
            jta.replaceRange("",jta.getText().length() - 1,jta.getText().length());
           }
         else
           {
            buffer.append(c);    
            jta.append(buffer.toString());
            buffer = new StringBuffer();
           }
         
         e.consume();
                  
         notify();
        }
     }
     
   /****************/
   /* getPriority: */
   /****************/
   public int getPriority()
     {
      return 10;
     }

   /************/
   /* getName: */
   /************/
   public String getName()
     {
      return "Router1Demo";
     }

   /**********/
   /* query: */
   /**********/
   public boolean query(
     String routerName)
     {      
      if (routerName.equals("stdout") ||
          routerName.equals("stdin") ||
          routerName.equals("wwarning") ||
          routerName.equals("werror") ||
          routerName.equals("wtrace") ||
          routerName.equals("wdialog") ||
          routerName.equals("wclips") ||
          routerName.equals("wdisplay"))
      
        { return true; }

      return false;
     }

   /**********/
   /* print: */
   /**********/
   public void print(
     String routerName,
     String printString)
     {
      readInputBuffer = null;
      readInputLength = 0;
      readInputPosition = 0;

      bufferAppend(printString);
     }

   /***********************/
   /* setReadInputBuffer: */
   /***********************/
   public synchronized void setReadInputBuffer(
     String bufferString)
     {
      if (! expectingInput) return;
      
      if (readInputBuffer == null)
        { 
         readInputBuffer = bufferString; 
         readInputLength = readInputBuffer.length();
         readInputPosition = 0;
        }
      else
        {
         readInputBuffer = readInputBuffer.concat(bufferString);
         readInputLength = readInputBuffer.length();
        }
        
      if (readInputPosition < readInputLength)
        {
         charInput = readInputBuffer.charAt(readInputPosition++);

         notify();
        }
      else
        {
         readInputBuffer = null; 
         readInputLength = 0;
         readInputPosition = 0;
        }
     }
  
   /*****************/
   /* bufferAppend: */
   /*****************/
   public synchronized void bufferAppend(
     String theString)
     {
      boolean invoke;

      if (buffer.length() != 0) 
        { invoke = false; }
      else
        { invoke = true; }
      
      buffer.append(theString); 

      if (! invoke) return;
      
      SwingUtilities.invokeLater(
         new Runnable()
           {
            public void run()
               {
                try 
                  { bufferUpdate(); }
                catch (Exception e)
                  { e.printStackTrace(); }
               }
           });
     }

   /*****************/
   /* bufferUpdate: */
   /*****************/
   public synchronized void bufferUpdate()
     {    
      jta.append(buffer.toString());
      buffer = new StringBuffer(); 
      
      if (jta.getLineCount() > maxLines)
        {
         try
           {
            int beginOffset = jta.getLineStartOffset(0);
            int endOffset = jta.getLineStartOffset(jta.getLineCount() - maxLines);
            jta.replaceRange("",beginOffset,endOffset);
           }
         catch (Exception e)
           { e.printStackTrace(); }
        }
     }
     
   /************/
   /* getchar: */
   /************/
   public int getchar(
     String routerName)
     {
      int rv = -1;
      int readInputChar = -1;
      
      if (ungotten)
        {
         ungotten = false;
         return lastChar;
        }
        
      if (curChar < numChar)
        {
         if (curChar == 1)
           { rv = char2; }
         else
           { rv = char3; }
         curChar++;
         
         return rv;
        }

      if ((readInputBuffer != null) &&
          (readInputPosition < readInputLength))
        { charInput = readInputBuffer.charAt(readInputPosition++); }
      else
        {
         synchronized(this)
           {
            expectingInput = true;
            bufferInputCount = clips.getInputBufferCount();
            try
              { 
               wait(); 
               expectingInput = false;
              }
            catch (Exception e)
              { e.printStackTrace(); }       
           }
        }
           
      if ((readInputBuffer != null) && 
          (readInputPosition >= readInputLength))
        {
         readInputBuffer = null;
         readInputPosition = 0;
         readInputLength = 0;
        }
        
      if (charInput <= 127)
        { 
         numChar = 1;
         curChar = 1;
         return charInput; 
        }
      else if ((charInput > 127) && (charInput < 2048)) 
        {
         numChar = 2;
         curChar = 1;
         char2 = ((charInput & 0x3F) | 0x80);
         charInput = (((charInput >> 6) & 0x1F) | 0xC0);
         return charInput;
        }
      else
        {
         numChar = 3;
         curChar = 1;
         char2 = (((charInput >> 6) & 0x3F) | 0x80);
         char3 = ((charInput & 0x3F) | 0x80);
         charInput = (((charInput >> 12) & 0x0F) | 0xE0);
         return charInput;
        }
     }

   /**************/
   /* ungetchar: */
   /**************/
   public int ungetchar(
     String routerName,
     int theChar)
     {
      if (ungotten)
        { return -1; }
        
      lastChar = theChar;
      ungotten = true;
      
      return theChar;
     }

   /*********/
   /* exit: */
   /*********/
   public boolean exit(
     int exitCode)
     {      
      return true;
     }
     
  }