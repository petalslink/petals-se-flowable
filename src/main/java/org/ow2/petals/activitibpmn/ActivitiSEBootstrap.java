/**
 * Copyright (c) 2014-2014 Linagora
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
package org.ow2.petals.activitibpmn;

import java.util.ArrayList;
import java.util.List;

import org.ow2.petals.component.framework.DefaultBootstrap;

/**
 * The component class of the Activiti BPMN Service Engine.
 * @author Bertrand Escudie - Linagora
 */
public class ActivitiSEBootstrap extends DefaultBootstrap {

	
	 /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.petals.component.framework.DefaultBootstrap#getAttributeList()
     */
    @Override
    public List<String> getAttributeList() {
        this.getLogger().info("***********************");
		this.getLogger().info("*** Start getAttributeList() in ActivitiSEBootstrap.");

        final List<String> attributes = new ArrayList<String>();
        
        attributes.add("jdbcDriver");
        attributes.add("jdbcUrl");
        attributes.add("jdbcUsername");
        attributes.add("jdbcPassword");
        attributes.add("jdbcMaxActiveConnections");
        attributes.add("jdbcMaxIdleConnections");
        attributes.add("jdbcMaxCheckoutTime");
        attributes.add("jdbcMaxWaitTime");
        attributes.add("databaseType");
        attributes.add("databaseSchemaUpdate");

        this.getLogger().info("*** End getAttributeList() in ActivitiSEBootstrap.");
        this.getLogger().info("***********************");
        
        return attributes;
    }
	
    /**
     * Get the jdbc Driver
     * 
     * @return the jdbc Driver
     */
    public String getJdbcDriver() {
    	return this.getParam(ActivitiSEConstants.DBServer.JDBC_DRIVER);
    }
 
    /**
     * Set the jdbc Driver
     * 
     * @param value the jdbc Driver
     */
    public void setJdbcDriver(final String value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_DRIVER, value);
    }
  
    /**
     * Get the jdbc URL
     * 
     * @return the jdbc URL
     */
    public String getJdbcUrl() {
    	return this.getParam(ActivitiSEConstants.DBServer.JDBC_URL);
    }
 
    /**
     * Set the jdbc URL
     * 
     * @param value the jdbc URL
     */
    public void setJdbcUrl(final String value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_URL, value);
    }
  
    /**
     * Get the jdbc User Name
     * 
     * @return the jdbc User Name
     */
    public String getJdbcUsername() {
    	return this.getParam(ActivitiSEConstants.DBServer.JDBC_USERNAME);
    }
 
    /**
     * Set the jdbc User Name
     * 
     * @param value the User Name
     */
    public void setJdbcUsername(final String value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_USERNAME, value);
    }
  
    /**
     * Get the jdbc Password
     * 
     * @return the jdbc Password
     */
    public String getJdbcPassword() {
    	return this.getParam(ActivitiSEConstants.DBServer.JDBC_PASSWORD);
    }
 
    /**
     * Set the jdbc UserName
     * 
     * @param value the UserName
     */
    public void setJdbcPassword(final String value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_PASSWORD, value);
    }
  
    /**
     * Get the jdbc MaxActiveConnections
     * 
     * @return the jdbc MaxActiveConnections
     */
    public int getJdbcMaxActiveConnections() {
        int jdbcMaxActiveConnections = 0;

        final String JdbcMaxActiveConnectionsString = this.getParam(ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
        if (JdbcMaxActiveConnectionsString != null) {
            jdbcMaxActiveConnections = Integer.parseInt(JdbcMaxActiveConnectionsString);
        }

        return jdbcMaxActiveConnections;
    }
 
    /**
     * Set the jdbc MaxActiveConnections
     * 
     * @param value the MaxActiveConnections
     */
    public void setJdbcMaxActiveConnections(final int value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS, Integer.toString(value));
    }
  
    /**
     * Get the jdbc MaxIdleConnections
     * 
     * @return the jdbc MaxIdleConnections
     */
    public int getJdbcMaxIdleConnections() {
        int jdbcMaxIdleConnections = 0;

        final String JdbcMaxIdleConnectionsString = this.getParam(ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
        if (JdbcMaxIdleConnectionsString != null) {
            jdbcMaxIdleConnections = Integer.parseInt(JdbcMaxIdleConnectionsString);
        }

        return jdbcMaxIdleConnections;
    }
 
    /**
     * Set the jdbc MaxIdleConnections
     * 
     * @param value the MaxIdleConnections
     */
    public void setJdbcMaxIdleConnections(final int value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS, Integer.toString(value));
    }
  
    /**
     * Get the jdbc MaxCheckoutTime
     * 
     * @return the jdbc MaxCheckoutTime
     */
    public int getJdbcMaxCheckoutTime() {
        int jdbcMaxCheckoutTime = 0;

        final String JdbcMaxCheckoutTimeString = this.getParam(ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
        if (JdbcMaxCheckoutTimeString != null) {
            jdbcMaxCheckoutTime = Integer.parseInt(JdbcMaxCheckoutTimeString);
        }

        return jdbcMaxCheckoutTime;
    }
 
    /**
     * Set the jdbc MaxCheckoutTime
     * 
     * @param value the MaxCheckoutTime
     */
    public void setJdbcMaxCheckoutTime(final int value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME, Integer.toString(value));
    }
 
    /**
     * Get the jdbc MaxWaitTime
     * 
     * @return the jdbc MaxWaitTime
     */
    public int getJdbcMaxWaitTime() {
        int jdbcMaxWaitTime = 0;

        final String JdbcMaxWaitTimeString = this.getParam(ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
        if (JdbcMaxWaitTimeString != null) {
            jdbcMaxWaitTime = Integer.parseInt(JdbcMaxWaitTimeString);
        }

        return jdbcMaxWaitTime;
    }
 
    /**
     * Set the jdbc MaxWaitTime
     * 
     * @param value the MaxWaitTime
     */
    public void setJdbcMaxWaitTime(final int value) {
        this.setParam(ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME, Integer.toString(value));
    }
 
    /**
     * Get the databaseType
     * 
     * @return the databaseType
     */
    public String getDatabaseType() {
    	return this.getParam(ActivitiSEConstants.DBServer.DATABASE_TYPE);
    }
     
    /**
     * Set the databaseType
     * 
     * @param value the databaseType
     */
    public void setDatabaseType(final String value) {
        this.setParam(ActivitiSEConstants.DBServer.DATABASE_TYPE, value);
    }
  
    /**
     * Get the databaseSchemaUpdate
     * 
     * @return the databaseSchemaUpdate
     */
    public String getDatabaseSchemaUpdate() {
    	return this.getParam(ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE);
    }
     
    /**
     * Set the databaseSchemaUpdate
     * 
     * @param value the databaseSchemaUpdate
     */
    public void setDatabaseSchemaUpdate(final String value) {
        this.setParam(ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE, value);
    }
     
}
