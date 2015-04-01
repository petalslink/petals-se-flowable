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
package org.ow2.petals.activitibpmn.monitoring;

import org.ow2.petals.component.framework.logger.ConsumeFlowStepBeginLogData;

/**
 * @author Christophe DENEUX - Linagora
 */
public final class ProcessInstanceFlowStepBeginLogData extends ConsumeFlowStepBeginLogData {

    private static final long serialVersionUID = -310582124340683531L;

    /**
     * Name of the MONIT trace attribute containing the process definition of the process instance created.
     */
    public static final String PROCESS_DEFINITION_KEY = "processDefinition";

    /**
     * Name of the MONIT trace attribute containing the identifier of process instance created.
     */
    public static final String PROCESS_INSTANCE_ID_KEY = "processInstanceId";

    /**
     * 
     * @param flowInstanceId
     *            The flow instance identifier used to create the process instance
     * @param flowStepId
     *            The flow step identifier of the task completing the user task
     * @param flowInterfaceName
     *            Interface name associated to the interaction request creating the process instance
     * @param flowServiceName
     *            Service name associated to the interaction request creating the process instance
     * @param flowEndpointName
     *            Endpoint name associated to the interaction request creating the process instance
     * @param flowOperationName
     *            operation name associated to the interaction request creating the process instance
     * @param correlatedFlowInstanceId
     *            The flow instance identifier of the correlated process completing the user task, ie. the flow instance
     *            identifier coming from the request completing the user task
     * @param correlatedFlowStepId
     *            The flow step identifier of the correlated process completing the user task, ie. the flow step
     *            identifier coming from the request completing the user task
     */
    public ProcessInstanceFlowStepBeginLogData(final String flowInstanceId, final String flowStepId,
            final String flowInterfaceName, final String flowServiceName, final String flowEndpointName,
            final String flowOperationName, final String correlatedFlowInstanceId, final String correlatedFlowStepId,
            final String processDefinition, final String processInstanceId) {

        super(flowInstanceId, flowStepId, flowInterfaceName, flowServiceName, flowEndpointName, flowOperationName);

        ActivitiActivityFlowStepData.addActivitiActivityFlowStepData(this, correlatedFlowInstanceId,
                correlatedFlowStepId);
        this.putData(PROCESS_DEFINITION_KEY, processDefinition);
        this.putData(PROCESS_INSTANCE_ID_KEY, processInstanceId);
    }
}
