/**
 * Copyright (c) 2015-2021 Linagora
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
package org.ow2.petals.flowable.incoming.operation.annotated.exception;

import javax.xml.namespace.QName;


/**
 * The output XSLT style-sheet declared with annotations is invalid.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class InvalidOutputXslException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = 7366083908530355735L;

    private static final String MESSAGE_PATTERN = "The output XSLT-stylesheet '%s' declared with annotations is invalid: %s. See previous errors logged.";

    /**
     * The output XSLT style-sheet that does not exist
     */
    private final String xslFileName;

    public InvalidOutputXslException(final QName wsdlOperation, final String xslFileName, final Throwable cause) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, xslFileName, cause.getMessage()), cause);
        this.xslFileName = xslFileName;
    }

    /**
     * @return The output XSLT style-sheet that does not exist
     */
    public String getXslFileName() {
        return this.xslFileName;
    }

}
