/**
 * Copyright (c) 2017-2018 Linagora
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
package org.ow2.petals.flowable.rest.config;

import org.flowable.rest.security.BasicAuthenticationProvider;
import org.ow2.petals.flowable.FlowableSE;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;

/**
 * This defines the REST API security by configuring the user group (from the Flowable engine identity service) that can
 * access the API. Inspired by flowable-rest-app code (org.flowable.rest.conf.SecurityConfiguration).
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
@Configuration
@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {

    public static final String FLOWABLE_REST_API_ACCESS_GROUP_QUALIFIER = "org.ow2.petals.flowable.rest.RestApiAccessGroup";

    /**
     * This is injected by the SE (see {@link FlowableSE#createRestApi()}).
     */
    @Autowired
    @Qualifier(FLOWABLE_REST_API_ACCESS_GROUP_QUALIFIER)
    private String accessGroup;

    @Bean
    public AuthenticationProvider authenticationProvider() {
        return new BasicAuthenticationProvider();
    }

    @Override
    protected void configure(final HttpSecurity http) throws Exception {
        final HttpSecurity httpSecurity = http.authenticationProvider(authenticationProvider()).sessionManagement()
                .sessionCreationPolicy(SessionCreationPolicy.STATELESS).and().csrf().disable();

        // let's protect ourselves from potential security risks!
        if (accessGroup == null || accessGroup.trim().isEmpty()) {
            throw new AssertionError("impossible");
        }

        httpSecurity.authorizeRequests().anyRequest().hasAuthority(accessGroup).and().httpBasic();
    }

}
