/**
 * Copyright (c) 2014 Linagora
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
package org.ow2.petals.activitibpmn.operation.annotated.exception;


/**
 * The BPMN annotation defining a BPMN operation is missing on a WSDL operation
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoBpmnOperationDefinedException extends InvalidAnnotationForOperationException {

    private static final long serialVersionUID = -397843616017052907L;

    private static final String MESSAGE = "The BPMN annotation defining a BPMN operation is missing";

    public NoBpmnOperationDefinedException(final String wsdlOperationName) {
        super(wsdlOperationName, MESSAGE);
    }

}
