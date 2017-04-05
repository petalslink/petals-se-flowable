/**
 * Copyright (c) 2015-2017 Linagora
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
public final class UserTaskFlowStepBeginLogData extends ProvideFlowStepBeginLogData {

    private static final long serialVersionUID = 1179141207903698624L;

    /**
     * Name of the MONIT trace attribute containing the definition identifier of the user task instance.
     */
    public static final String TASK_DEFINITION_KEY = "taskDefinition";

    /**
     * Name of the MONIT trace attribute containing the instance identifier of the user task instance.
     */
    public static final String TASK_INSTANCE_ID_KEY = "taskInstanceId";

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier of the task completing the user task
     * @param flowPreviousStepId
     *            The flow step identifier of the calling task/step
     * @param taskDefinitionKey
     *            The definition identifier of the current user task
     * @param taskInstanceId
     *            The instance identifier of the current user task
     */
    public UserTaskFlowStepBeginLogData(final String flowInstanceId, final String flowStepId,
            final String flowPreviousStepId, final String taskDefinitionKey, final String taskInstanceId) {

        // TODO: Remove "new MessageExchangeImpl()" when a dedicated step trace will be created
        super(flowInstanceId, flowStepId, null, null, null, null, flowPreviousStepId,
                new MessageExchangeImpl("", MEPPatternConstants.IN_OUT.value(), new Location("", "")));
        this.putData(TASK_DEFINITION_KEY, taskDefinitionKey);
        this.putData(TASK_INSTANCE_ID_KEY, taskInstanceId);

    }
}
