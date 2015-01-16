/**
 * Copyright (c) 2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.operation.annotated.exception;

import javax.xml.xpath.XPathExpressionException;

/**
 * The expression defining the process instance identifier is invalid.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessInstanceIdMappingExpressionException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -923411192940973985L;

    private static final String MESSAGE_PATTERN = "The mapping defining the process instance identifier is invalid: %s";

    public ProcessInstanceIdMappingExpressionException(final String wsdlOperationName, final XPathExpressionException cause) {
        super(wsdlOperationName, String.format(MESSAGE_PATTERN, cause.getMessage()), cause);
    }

}
