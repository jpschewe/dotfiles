package jde.debugger;

/**
 * JDENumberFormatException.java
 * <p>
 * 
 * <p>
 * Created: Thu Aug  5 18:52:41 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.1 $
 */

public class JDENumberFormatException extends JDEException {
    
  public JDENumberFormatException(String str) {
    super("Non-numeric "+str);
  }
    
} // JDENumberFormatException

/*
 * $Log: JDENumberFormatException.java,v $
 * Revision 1.1  2004/11/14 18:19:03  jpschewe
 * Updated to JDEE 2.3.4.
 *
 * Revision 1.2  2003/01/08 06:53:37  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// end of JDENumberFormatException.java
