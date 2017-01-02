/**
 * Copyright (c) 2015-2016 Linagora
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
package org.ow2.petals.activitibpmn.incoming.operation.exception;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.ow2.petals.activitibpmn.incoming.operation.ActivitiOperation;

/**
 * The process instance on which an operation is asked is not found in the BPMN engine
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessInstanceNotFoundException extends OperationProcessingFault {

    private static final long serialVersionUID = 2116484350159165117L;

    private static final String MESSAGE_PATTERN = "The process instance '%s' is not found in the BPMN engine";

    /**
     * The identifier of the process instance not found
     */
    private final String processInstanceId;

    /**
     * @param processInstanceId
     *            The identifier of the process instance not found
     */
    public ProcessInstanceNotFoundException(final QName wsdlOperation, final String processInstanceId) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, processInstanceId));
        this.processInstanceId = processInstanceId;
    }

    /**
     * @return The identifier of the process instance not found
     */
    public String getProcessInstanceId() {
        return this.processInstanceId;
    }

    @Override
    public Map<QName, String> getXslParameters() {
        final Map<QName, String> xslParameters = new HashMap<>();
        xslParameters.put(new QName(ActivitiOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS,
                ActivitiOperation.SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), this.processInstanceId);
        return xslParameters;
    }
}
