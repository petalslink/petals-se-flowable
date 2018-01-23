/**
 * Copyright (c) 2015-2018 Linagora
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
 * The output XSLT style-sheet declared with annotations does not exist in service-unit.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class OutputXslNotFoundException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -9169248969557869163L;

    private static final String MESSAGE_PATTERN = "The output XSLT-stylesheet '%s' declared with annotations does not exist into the service unit";

    /**
     * The output XSLT style-sheet that does not exist
     */
    private final String xslFileName;

    public OutputXslNotFoundException(final QName wsdlOperation, final String xslFileName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, xslFileName));
        this.xslFileName = xslFileName;
    }

    /**
     * @return The output XSLT style-sheet that does not exist
     */
    public String getXslFileName() {
        return this.xslFileName;
    }

}
