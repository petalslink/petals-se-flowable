/**
 * Copyright (c) 2017-2022 Linagora
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
package org.ow2.petals.flowable.incoming.operation.exception;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.ow2.petals.flowable.incoming.operation.FlowableOperation;

/**
 * The process instance is ended, so no intermediate message is expected.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessInstanceEndedException extends OperationProcessingFault {

    private static final long serialVersionUID = -3341958102272906016L;

    private static final String MESSAGE_PATTERN = "The process instance '%s' is ended, so no intermediate message event '%s' is expected.";

    /**
     * The message name of the intermediate message event already received
     */
    private final String messageEventName;

    /**
     * The identifier of the process instance for which the intermediate message was already received
     */
    private final String processInstanceId;

    /**
     * @param processInstanceId
     *            The identifier of the process instance for which the intermediate message was already received
     * @param messageEventName
     *            The message name of the intermediate message event already received
     */
    public ProcessInstanceEndedException(final QName wsdlOperation, final String processInstanceId,
            final String messageEventName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, processInstanceId, messageEventName));
        this.processInstanceId = processInstanceId;
        this.messageEventName = messageEventName;
    }

    /**
     * @return The message name of the intermediate message event already received
     */
    public String getMessageEventName() {
        return this.messageEventName;
    }

    /**
     * @return The identifier of the process instance for which the intermediate message was already received
     */
    public String getProcessInstanceId() {
        return this.processInstanceId;
    }

    @Override
    public Map<QName, String> getXslParameters() {
        final Map<QName, String> xslParameters = new HashMap<>();
        xslParameters.put(new QName(FlowableOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS,
                FlowableOperation.SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID), this.processInstanceId);
        xslParameters.put(new QName(FlowableOperation.SCHEMA_OUTPUT_XSLT_FAULT_PARAMS,
                FlowableOperation.SCHEMA_OUTPUT_XSLT_PARAM_MESSAGE_NAME), this.messageEventName);
        return xslParameters;
    }
}
