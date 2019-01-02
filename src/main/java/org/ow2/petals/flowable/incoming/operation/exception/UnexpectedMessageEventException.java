/**
 * Copyright (c) 2017-2019 Linagora
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
 * The intermediate message received by the BPMN engine to complete an intermediate message catch event is not expected on
 * BPMN engine side.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class UnexpectedMessageEventException extends OperationProcessingFault {

    private static final long serialVersionUID = 8306319375479683479L;

    private static final String MESSAGE_PATTERN = "Unexpected intermediate message event '%s' on the process instance '%s' by the BPMN engine";

    /**
     * The message name of the intermediate message not expected
     */
    private final String messageEventName;

    /**
     * The identifier of the process instance for which the intermediate message is not expected
     */
    private final String processInstanceId;

    /**
     * @param processInstanceId
     *            The identifier of the process instance for which the intermediate message is not expected
     * @param messageEventName
     *            The message name of the intermediate message event not expected
     */
    public UnexpectedMessageEventException(final QName wsdlOperation, final String processInstanceId,
            final String messageEventName) {
        super(wsdlOperation, String.format(MESSAGE_PATTERN, messageEventName, processInstanceId));
        this.processInstanceId = processInstanceId;
        this.messageEventName = messageEventName;
    }

    /**
     * @return The message name of the intermediate message event not expected
     */
    public String getMessageEventName() {
        return this.messageEventName;
    }

    /**
     * @return The identifier of the process instance for which the intermediate message is not expected
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
