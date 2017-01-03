/**
 * Copyright (c) 2015-2017 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.operation.annotated.exception;

import javax.xml.namespace.QName;

/**
 * The fault XSLT style-sheet declared with annotations does not exist in service-unit.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class FaultXslNotFoundException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 4056847614730597369L;

    private static final String MESSAGE_PATTERN = "The fault XSLT-stylesheet '%s' declared with annotations for the WSDL fault '%s' does not exist into the service unit";

    /**
     * The output XSLT style-sheet that does not exist
     */
    private final String xslFileName;

    /**
     * Name of the WSDL fault into the WSDL operation for which an error occurs
     */
    private final String wsdlFault;

    public FaultXslNotFoundException(final QName wsdlOperation, final String wsdlFault, final String xslFileName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, xslFileName, wsdlFault));
        this.xslFileName = xslFileName;
        this.wsdlFault = wsdlFault;
    }

    /**
     * @return The output XSLT style-sheet that does not exist
     */
    public String getXslFileName() {
        return this.xslFileName;
    }

    /**
     * @return The name of the WSDL fault into the WSDL operation for which an error occurs
     */
    public String getWsdlFault() {
        return this.wsdlFault;
    }

}
