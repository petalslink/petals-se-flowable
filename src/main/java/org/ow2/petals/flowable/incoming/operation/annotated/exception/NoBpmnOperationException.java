/**
 * Copyright (c) 2015-2026 Linagora
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


/**
 * No BPMN operation is defined in the WSDL
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoBpmnOperationException extends InvalidAnnotationException {

    private static final long serialVersionUID = 4062096690326440564L;

    private static final String MESSAGE = "No BPMN operation declared in the WSDL";

    public NoBpmnOperationException() {
        super(MESSAGE);
    }
}
