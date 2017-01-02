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
package org.ow2.petals.activitibpmn.event;

import java.util.Map;
import java.util.logging.Logger;

import org.activiti.engine.delegate.event.ActivitiActivityEvent;
import org.activiti.engine.delegate.event.ActivitiEvent;
import org.activiti.engine.delegate.event.ActivitiEventListener;
import org.activiti.engine.delegate.event.ActivitiEventType;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.activitibpmn.ActivitiSEConstants;
import org.ow2.petals.activitibpmn.outgoing.cxf.transport.PetalsConduit;
import org.ow2.petals.commons.log.FlowAttributes;

/**
 * The event listener fired when a service task is started to set flow attributes in a context to be retrieved by the
 * Petals transporter to set them in the Message exchange sent to the Petals service associated to the service task.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ServiceTaskStartedEventListener extends AbstractEventListener implements
        ActivitiEventListener {

    public ServiceTaskStartedEventListener(final Logger log) {
        super(ActivitiEventType.ACTIVITY_STARTED, log);
    }

    @Override
    protected void processEvent(final ActivitiEvent event) {

        final ActivitiActivityEvent eventImpl = (ActivitiActivityEvent) event;
        if ("serviceTask".equals(eventImpl.getActivityType())) {

            final ProcessInstanceQuery processInstanceQuery = event.getEngineServices().getRuntimeService()
                    .createProcessInstanceQuery().processInstanceId(event.getProcessInstanceId())
                    .includeProcessVariables();
            final ProcessInstance processInstance = processInstanceQuery.singleResult();

            final Map<String, Object> processVariables = processInstance.getProcessVariables();

            final String flowInstanceId = (String) processVariables
                    .get(ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_INSTANCE_ID);
            final String flowStepId = (String) processVariables
                    .get(ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_STEP_ID);

            PetalsConduit.flowAttributes.set(new FlowAttributes(flowInstanceId, flowStepId));
        }

    }
}
