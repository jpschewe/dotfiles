package jde.debugger.spec;

import com.sun.jdi.*;
import jde.debugger.Protocol;

/**
 * ReferenceTypeSpec.java
 *
 *
 * Created: Mon Jul 19 13:19:23 1999
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 *  
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.1 $
 */

public interface ReferenceTypeSpec extends Protocol {

    /**
     * @return true if the ref type matches this spec
     */
  public boolean matches(ReferenceType refType);

}

/*
 * $Log: ReferenceTypeSpec.java,v $
 * Revision 1.1  2004/11/14 18:19:02  jpschewe
 * Updated to JDEE 2.3.4.
 *
 * Revision 1.2  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of ReferenceTypeSpec.java
