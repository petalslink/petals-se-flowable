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
package org.ow2.petals.activitibpmn.event;

import java.util.Map;
import java.util.logging.Logger;

import org.activiti.engine.delegate.event.ActivitiEntityEvent;
import org.activiti.engine.delegate.event.ActivitiEvent;
import org.activiti.engine.delegate.event.ActivitiEventListener;
import org.activiti.engine.delegate.event.ActivitiEventType;
import org.activiti.engine.impl.persistence.entity.TaskEntity;
import org.ow2.petals.activitibpmn.ActivitiSEConstants;
import org.ow2.petals.activitibpmn.monitoring.UserTaskFlowStepBeginLogData;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * <p>
 * The event listener fired when a user task is started.
 * </p>
 * <p>
 * This event listener is in charge of logging the associated MONIT trace. The flow attributes are retrieved from the
 * process variables and task variables.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UserTaskStartedEventListener extends AbstractMonitDirectLoggerEventListener implements
        ActivitiEventListener {

    private final SimpleUUIDGenerator simpleUUIDGenerator;

    public UserTaskStartedEventListener(final SimpleUUIDGenerator simpleUUIDGenerator, final Logger log) {
        super(ActivitiEventType.TASK_CREATED, log);
        this.simpleUUIDGenerator = simpleUUIDGenerator;
    }

    @Override
    protected AbstractFlowLogData createLogData(final ActivitiEvent event) {
        
        if (event instanceof ActivitiEntityEvent) {
            final ActivitiEntityEvent entityEvent = (ActivitiEntityEvent)event;
            final Object entity = entityEvent.getEntity();
            if (entity instanceof TaskEntity) {
                final TaskEntity taskEntity = (TaskEntity)entity;
                
                final Map<String, Object> processVariables = taskEntity.getActivityInstanceVariables();

                final String flowInstanceId = (String) processVariables
                        .get(ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_INSTANCE_ID);
                // TODO: Review the value of the flow previous step identifier to manage correctly process with branches
                final String flowPreviousStepId = (String) processVariables
                        .get(ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_STEP_ID);
                
                final String flowStepId = this.simpleUUIDGenerator.getNewID();

                // We store the flow step id as task local variable
                event.getEngineServices()
                        .getTaskService()
                        .setVariableLocal(taskEntity.getId(), ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_STEP_ID,
                                flowStepId);

                return new UserTaskFlowStepBeginLogData(flowInstanceId, flowStepId, flowPreviousStepId, taskEntity
                        .getTaskDefinition().getKey(), taskEntity.getId());
            }
        }
        
        return null;

    }
}
