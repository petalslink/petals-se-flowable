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
package org.ow2.petals.activitibpmn.event;

import java.util.Map;
import java.util.logging.Logger;

import org.activiti.engine.delegate.event.ActivitiEntityWithVariablesEvent;
import org.activiti.engine.delegate.event.ActivitiEvent;
import org.activiti.engine.delegate.event.ActivitiEventListener;
import org.activiti.engine.delegate.event.ActivitiEventType;
import org.activiti.engine.impl.persistence.entity.ExecutionEntity;
import org.ow2.petals.activitibpmn.ActivitiSEConstants;
import org.ow2.petals.activitibpmn.monitoring.ProcessInstanceFlowStepBeginLogData;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * The event listener fired when a process instance is started to log the MONIT trace.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ProcessInstanceStartedEventListener extends AbstractMonitDirectLoggerEventListener implements
        ActivitiEventListener {

    public ProcessInstanceStartedEventListener(final Logger log) {
        super(ActivitiEventType.PROCESS_INSTANCE_STARTED, log);
    }

    @Override
    protected AbstractFlowLogData createLogData(final ActivitiEvent event) {

        final String processInstanceId = event.getProcessInstanceId();
        this.log.fine("The process instance '" + processInstanceId + "' is started.");

        if (event instanceof ActivitiEntityWithVariablesEvent) {
            final ActivitiEntityWithVariablesEvent eventImpl = (ActivitiEntityWithVariablesEvent) event;

                final Map<String, Object> processVariables = eventImpl.getVariables();

                final String flowInstanceId = (String) processVariables
                        .get(ActivitiSEConstants.Activiti.PROCESS_VAR_PETALS_FLOW_INSTANCE_ID);
                final String flowStepId = (String) processVariables
                        .get(ActivitiSEConstants.Activiti.PROCESS_VAR_PETALS_FLOW_STEP_ID);
                final String correlatedFlowInstanceId = (String) processVariables
                        .get(ActivitiSEConstants.Activiti.PROCESS_VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                final String correlatedFlowStepId = (String) processVariables
                        .get(ActivitiSEConstants.Activiti.PROCESS_VAR_PETALS_CORRELATED_FLOW_STEP_ID);

                final ExecutionEntity entity = (ExecutionEntity) eventImpl.getEntity();

                // TODO: Add a unit test where the process does not contain user task (the process instance is
                // completely executed when creating it) to demonstrate that the following MONIT trace occurs before the
                // trace 'consumeEnd'
                return new ProcessInstanceFlowStepBeginLogData(flowInstanceId, flowStepId, correlatedFlowInstanceId,
                        correlatedFlowStepId, entity.getProcessDefinition().getKey(), processInstanceId);

        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
            return null;
        }

    }
}
