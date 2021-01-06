/**
 * Copyright (c) 2017-2021 Linagora
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
package org.ow2.petals.flowable.monitoring;

import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.component.framework.logger.ProvideFlowStepBeginLogData;
import org.ow2.petals.jbi.messaging.exchange.impl.MessageExchangeImpl;
import org.ow2.petals.jbi.servicedesc.endpoint.Location;

/**
 * @author Christophe DENEUX - Linagora
 */
public final class IntermediateCatchMessageEventFlowStepBeginLogData extends ProvideFlowStepBeginLogData {

    private static final long serialVersionUID = 6525357770223936509L;

    /**
     * Name of the MONIT trace attribute containing the message name of the intermediate catch event.
     */
    public static final String MESSAGE_NAME = "messageName";

    /**
     * Name of the MONIT trace attribute containing the instance identifier of the intermediate catch message event
     * execution.
     */
    public static final String INTERMEDIATE_CATCH_MESSAGE_EVENT_INSTANCE_ID = "intermediateCatchMessageEventInstanceId";

    /**
     * Name of the MONIT trace attribute containing the instance identifier of the intermediate catch message event.
     */
    public static final String INTERMEDIATE_CATCH_MESSAGE_EVENT_ID = "intermediateCatchMessageEventId";

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier associated to the intermediate catch message event execution
     * @param flowPreviousStepId
     *            The flow step identifier of the previoius step
     * @param intermediateCatchMessageEventId
     *            The identifier of the current intermediate catch message event.
     * @param messageName
     *            The message name of the current intermediate catch event
     * @param intermediateCatchMessageEventId
     *            The instance identifier of the current intermediate catch message event.
     */
    public IntermediateCatchMessageEventFlowStepBeginLogData(final String flowInstanceId, final String flowStepId,
            final String flowPreviousStepId, final String intermediateCatchMessageEventId, final String messageName,
            final String intermediateCatchMessageEventInstanceId) {

        // TODO: Remove "new MessageExchangeImpl()" when a dedicated step trace will be created
        super(flowInstanceId, flowStepId, null, null, null, null, flowPreviousStepId,
                new MessageExchangeImpl("", MEPPatternConstants.IN_OUT.value(), new Location("", "")));
        this.putData(INTERMEDIATE_CATCH_MESSAGE_EVENT_ID, intermediateCatchMessageEventId);
        this.putData(MESSAGE_NAME, messageName);
        this.putData(INTERMEDIATE_CATCH_MESSAGE_EVENT_INSTANCE_ID, intermediateCatchMessageEventInstanceId);

    }
}
