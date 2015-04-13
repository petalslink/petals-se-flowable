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

import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.activiti.engine.delegate.event.ActivitiEntityEvent;
import org.activiti.engine.delegate.event.ActivitiEvent;
import org.activiti.engine.delegate.event.ActivitiEventListener;
import org.activiti.engine.delegate.event.ActivitiEventType;
import org.activiti.engine.impl.persistence.entity.TaskEntity;
import org.activiti.engine.task.Task;
import org.ow2.petals.activitibpmn.ActivitiSEConstants;
import org.ow2.petals.activitibpmn.monitoring.UserTaskFlowStepEndLogData;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * <p>
 * The event listener fired when a user task is completed.
 * </p>
 * <p>
 * This event listener is in charge of logging the associated MONIT trace. The flow attributes are retrieved from the
 * process variables and task variables.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UserTaskCompletedEventListener extends AbstractMonitDirectLoggerEventListener implements
        ActivitiEventListener {

    public UserTaskCompletedEventListener(final Logger log) {
        super(ActivitiEventType.TASK_COMPLETED, log);
    }

    @Override
    protected AbstractFlowLogData createLogData(final ActivitiEvent event) {
        
        if (event instanceof ActivitiEntityEvent) {
            final ActivitiEntityEvent entityEvent = (ActivitiEntityEvent)event;
            final Object entity = entityEvent.getEntity();
            if (entity instanceof TaskEntity) {
                final TaskEntity taskEntity = (TaskEntity)entity;
                
                final Map<String, Object> processVariables = taskEntity.getActivityInstanceVariables();

                final List<Task> tasks = event.getEngineServices().getTaskService().createTaskQuery()
                        .taskId(taskEntity.getId()).includeTaskLocalVariables().list();
                if (tasks.size() > 0) {
                    final Map<String, Object> tasklocalVariables = tasks.get(0).getTaskLocalVariables();

                    final String flowInstanceId = (String) processVariables
                            .get(ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_INSTANCE_ID);

                    final String flowStepId = (String) tasklocalVariables
                            .get(ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_STEP_ID);
                    final String correlatedFlowInstanceId = (String) tasklocalVariables
                            .get(ActivitiSEConstants.Activiti.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                    final String correlatedFlowStepId = (String) tasklocalVariables
                            .get(ActivitiSEConstants.Activiti.VAR_PETALS_CORRELATED_FLOW_STEP_ID);

                    return new UserTaskFlowStepEndLogData(flowInstanceId, flowStepId, correlatedFlowInstanceId,
                            correlatedFlowStepId);
                } else {
                    return null;
                }
            }
        }
        
        return null;

    }
}
