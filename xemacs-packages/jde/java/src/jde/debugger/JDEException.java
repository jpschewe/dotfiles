package jde.debugger;

/**
 * JDEException.java
 * <p>
 * This is the standard exception raised whenever something wrong happens...
 * The only thing worth mentioning is that the message is a description of
 * what went wrong.
 * <p>
 * Created: Fri Jul  9 10:55:21 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.1 $
 */

public class JDEException extends Exception implements Protocol {

  final String message;
    
  public JDEException(String str) {
    super(str);
    this.message = str;
  }

  public String getMessage() { return message; }

} // JDEException

/*
 * $Log: JDEException.java,v $
 * Revision 1.1  2004/11/14 18:19:03  jpschewe
 * Updated to JDEE 2.3.4.
 *
 * Revision 1.2  2003/01/08 06:53:38  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of JDEException
