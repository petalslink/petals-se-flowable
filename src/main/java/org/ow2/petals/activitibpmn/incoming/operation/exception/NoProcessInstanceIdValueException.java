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
package org.ow2.petals.activitibpmn.incoming.operation.exception;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;


/**
 * The value of the process instance identifier is required in the incoming request.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoProcessInstanceIdValueException extends OperationProcessingFault {

    private static final long serialVersionUID = 5318980517059515822L;

    private static final String MESSAGE = "Process instance identifier is missing or empty in the incoming request !";

    public NoProcessInstanceIdValueException(final String wsdlOperationName) {
        super(wsdlOperationName, MESSAGE);
    }

    @Override
    public Map<QName, String> getXslParameters() {
        final Map<QName, String> xslParameters = new HashMap<QName, String>();
        return xslParameters;
    }

}
